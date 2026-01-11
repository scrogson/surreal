//! Dependency management for Dream projects.
//!
//! Handles fetching dependencies from hex.pm, git repositories, and local paths.

use crate::config::{Dependency, ProjectConfig};
use flate2::read::GzDecoder;
use futures::future::join_all;
use reqwest::Client;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use tar::Archive;

/// Error type for dependency operations.
#[derive(Debug)]
pub struct DepsError {
    pub message: String,
}

impl DepsError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for DepsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for DepsError {}

pub type DepsResult<T> = Result<T, DepsError>;

/// Hex.pm repository base URL
const HEX_REPO: &str = "https://repo.hex.pm";

/// Dependency manager for a Dream project.
pub struct DepsManager {
    project_root: PathBuf,
    config: ProjectConfig,
    client: Client,
}

impl DepsManager {
    /// Create a new dependency manager.
    pub fn new(project_root: PathBuf, config: ProjectConfig) -> Self {
        Self {
            project_root,
            config,
            client: Client::new(),
        }
    }

    /// Get the deps directory path.
    pub fn deps_dir(&self) -> PathBuf {
        self.project_root.join("deps")
    }

    /// Get the build directory for compiled dependencies.
    pub fn deps_build_dir(&self) -> PathBuf {
        self.project_root.join("_build").join("dev").join("lib")
    }

    /// Fetch all dependencies.
    pub async fn fetch_all(&self) -> DepsResult<()> {
        if self.config.dependencies.is_empty() {
            println!("No dependencies to fetch.");
            return Ok(());
        }

        // Create deps directory
        let deps_dir = self.deps_dir();
        fs::create_dir_all(&deps_dir).map_err(|e| {
            DepsError::new(format!("Failed to create deps directory: {}", e))
        })?;

        println!("Fetching {} dependencies...", self.config.dependencies.len());

        // Fetch all hex dependencies concurrently
        let hex_deps: Vec<_> = self
            .config
            .dependencies
            .iter()
            .filter(|(_, dep)| dep.version().is_some() && !dep.is_git() && !dep.is_path())
            .collect();

        let fetch_futures: Vec<_> = hex_deps
            .iter()
            .map(|(name, dep)| self.fetch_hex_package(name, dep.version().unwrap()))
            .collect();

        let results = join_all(fetch_futures).await;

        // Check for errors
        for (i, result) in results.into_iter().enumerate() {
            if let Err(e) = result {
                let (name, _) = hex_deps[i];
                eprintln!("  Failed to fetch {}: {}", name, e);
            }
        }

        // Handle git dependencies sequentially (git clone isn't easily parallelized)
        for (name, dep) in &self.config.dependencies {
            if let Some(git_url) = dep.git_url() {
                self.fetch_git_package(name, git_url, dep)?;
            }
        }

        // Handle path dependencies (just symlink or copy)
        for (name, dep) in &self.config.dependencies {
            if let Some(path) = dep.path() {
                self.link_path_package(name, path)?;
            }
        }

        println!("Dependencies fetched successfully.");
        Ok(())
    }

    /// Fetch a package from hex.pm.
    async fn fetch_hex_package(&self, name: &str, version: &str) -> DepsResult<()> {
        let deps_dir = self.deps_dir();
        let pkg_dir = deps_dir.join(name);

        // Skip if already fetched
        if pkg_dir.exists() {
            println!("  {} {} (cached)", name, version);
            return Ok(());
        }

        println!("  Fetching {} {}...", name, version);

        // Download the tarball
        let url = format!("{}/tarballs/{}-{}.tar", HEX_REPO, name, version);
        let response = self
            .client
            .get(&url)
            .send()
            .await
            .map_err(|e| DepsError::new(format!("Failed to download {}: {}", name, e)))?;

        if !response.status().is_success() {
            return Err(DepsError::new(format!(
                "Failed to download {}: HTTP {}",
                name,
                response.status()
            )));
        }

        let tarball = response
            .bytes()
            .await
            .map_err(|e| DepsError::new(format!("Failed to read response for {}: {}", name, e)))?;

        // Extract the package
        self.extract_hex_tarball(name, &tarball)?;

        println!("  {} {} fetched", name, version);
        Ok(())
    }

    /// Extract a hex package tarball.
    fn extract_hex_tarball(&self, name: &str, tarball: &[u8]) -> DepsResult<()> {
        let deps_dir = self.deps_dir();
        let pkg_dir = deps_dir.join(name);

        // Create a temporary directory for extraction
        let temp_dir = deps_dir.join(format!(".{}-extract", name));
        fs::create_dir_all(&temp_dir).map_err(|e| {
            DepsError::new(format!("Failed to create temp directory: {}", e))
        })?;

        // The hex tarball contains: VERSION, metadata.config, contents.tar.gz, CHECKSUM
        let mut archive = Archive::new(tarball);

        let mut contents_tar_gz: Option<Vec<u8>> = None;

        for entry in archive.entries().map_err(|e| {
            DepsError::new(format!("Failed to read tarball for {}: {}", name, e))
        })? {
            let mut entry = entry.map_err(|e| {
                DepsError::new(format!("Failed to read tarball entry for {}: {}", name, e))
            })?;

            let path = entry.path().map_err(|e| {
                DepsError::new(format!("Failed to get entry path for {}: {}", name, e))
            })?;

            if path.to_string_lossy() == "contents.tar.gz" {
                let mut data = Vec::new();
                entry.read_to_end(&mut data).map_err(|e| {
                    DepsError::new(format!("Failed to read contents.tar.gz for {}: {}", name, e))
                })?;
                contents_tar_gz = Some(data);
            }
        }

        let contents_data = contents_tar_gz.ok_or_else(|| {
            DepsError::new(format!("No contents.tar.gz found in {} tarball", name))
        })?;

        // Extract contents.tar.gz to the package directory
        fs::create_dir_all(&pkg_dir).map_err(|e| {
            DepsError::new(format!("Failed to create package directory for {}: {}", name, e))
        })?;

        let decoder = GzDecoder::new(&contents_data[..]);
        let mut archive = Archive::new(decoder);
        archive.unpack(&pkg_dir).map_err(|e| {
            DepsError::new(format!("Failed to extract contents for {}: {}", name, e))
        })?;

        // Clean up temp directory
        let _ = fs::remove_dir_all(&temp_dir);

        Ok(())
    }

    /// Fetch a package from a git repository.
    fn fetch_git_package(&self, name: &str, url: &str, dep: &Dependency) -> DepsResult<()> {
        let deps_dir = self.deps_dir();
        let pkg_dir = deps_dir.join(name);

        // Skip if already fetched
        if pkg_dir.exists() {
            println!("  {} (git, cached)", name);
            return Ok(());
        }

        println!("  Cloning {} from {}...", name, url);

        // Get branch/tag/ref if specified
        let checkout_ref = if let Dependency::Detailed(spec) = dep {
            spec.branch
                .as_deref()
                .or(spec.tag.as_deref())
                .or(spec.git_ref.as_deref())
        } else {
            None
        };

        // Clone the repository
        let mut cmd = std::process::Command::new("git");
        cmd.arg("clone").arg("--depth").arg("1");

        if let Some(ref_name) = checkout_ref {
            cmd.arg("--branch").arg(ref_name);
        }

        cmd.arg(url).arg(&pkg_dir);

        let status = cmd.status().map_err(|e| {
            DepsError::new(format!("Failed to clone {}: {}", name, e))
        })?;

        if !status.success() {
            return Err(DepsError::new(format!("Git clone failed for {}", name)));
        }

        println!("  {} cloned", name);
        Ok(())
    }

    /// Link a local path dependency.
    fn link_path_package(&self, name: &str, path: &str) -> DepsResult<()> {
        let deps_dir = self.deps_dir();
        let pkg_dir = deps_dir.join(name);

        // Skip if already linked
        if pkg_dir.exists() {
            println!("  {} (path, cached)", name);
            return Ok(());
        }

        // Resolve the path relative to project root
        let source_path = if Path::new(path).is_absolute() {
            PathBuf::from(path)
        } else {
            self.project_root.join(path)
        };

        if !source_path.exists() {
            return Err(DepsError::new(format!(
                "Path dependency {} not found: {}",
                name,
                source_path.display()
            )));
        }

        println!("  Linking {} from {}...", name, path);

        // Create a symlink
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(&source_path, &pkg_dir).map_err(|e| {
                DepsError::new(format!("Failed to symlink {}: {}", name, e))
            })?;
        }

        #[cfg(windows)]
        {
            // On Windows, use junction for directories
            std::os::windows::fs::symlink_dir(&source_path, &pkg_dir).map_err(|e| {
                DepsError::new(format!("Failed to symlink {}: {}", name, e))
            })?;
        }

        println!("  {} linked", name);
        Ok(())
    }

    /// Compile all Erlang dependencies.
    pub fn compile_deps(&self) -> DepsResult<()> {
        let deps_dir = self.deps_dir();

        if !deps_dir.exists() {
            return Ok(());
        }

        println!("Compiling dependencies...");

        for entry in fs::read_dir(&deps_dir).map_err(|e| {
            DepsError::new(format!("Failed to read deps directory: {}", e))
        })? {
            let entry = entry.map_err(|e| {
                DepsError::new(format!("Failed to read deps entry: {}", e))
            })?;

            let pkg_name = entry.file_name().to_string_lossy().to_string();

            // Skip hidden directories
            if pkg_name.starts_with('.') {
                continue;
            }

            self.compile_erlang_dep(&pkg_name)?;
            self.compile_elixir_dep(&pkg_name)?;
        }

        println!("Dependencies compiled.");
        Ok(())
    }

    /// Compile an Erlang dependency.
    fn compile_erlang_dep(&self, name: &str) -> DepsResult<()> {
        let deps_dir = self.deps_dir();
        let pkg_dir = deps_dir.join(name);
        let src_dir = pkg_dir.join("src");
        let ebin_dir = pkg_dir.join("ebin");

        // Check if there are .erl files to compile
        if !src_dir.exists() {
            // Might be a Dream or Elixir package, skip for now
            return Ok(());
        }

        let erl_files: Vec<_> = fs::read_dir(&src_dir)
            .map_err(|e| DepsError::new(format!("Failed to read src dir for {}: {}", name, e)))?
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map_or(false, |ext| ext == "erl"))
            .collect();

        if erl_files.is_empty() {
            return Ok(());
        }

        println!("  Compiling {}...", name);

        // Create ebin directory
        fs::create_dir_all(&ebin_dir).map_err(|e| {
            DepsError::new(format!("Failed to create ebin for {}: {}", name, e))
        })?;

        // Compile each .erl file
        for entry in erl_files {
            let erl_path = entry.path();

            let status = std::process::Command::new("erlc")
                .arg("-o")
                .arg(&ebin_dir)
                .arg(&erl_path)
                .status()
                .map_err(|e| {
                    DepsError::new(format!("Failed to run erlc for {}: {}", name, e))
                })?;

            if !status.success() {
                return Err(DepsError::new(format!(
                    "Failed to compile {} in {}",
                    erl_path.display(),
                    name
                )));
            }
        }

        println!("  {} compiled", name);
        Ok(())
    }

    /// Compile an Elixir dependency.
    fn compile_elixir_dep(&self, name: &str) -> DepsResult<()> {
        let deps_dir = self.deps_dir();
        let pkg_dir = deps_dir.join(name);
        let lib_dir = pkg_dir.join("lib");
        let ebin_dir = pkg_dir.join("ebin");

        // Check if there are .ex files to compile
        if !lib_dir.exists() {
            return Ok(());
        }

        let ex_files: Vec<_> = Self::find_ex_files(&lib_dir);

        if ex_files.is_empty() {
            return Ok(());
        }

        // Check if elixirc is available
        let elixirc_check = std::process::Command::new("elixirc")
            .arg("--version")
            .output();

        if elixirc_check.is_err() {
            println!("  Skipping {} (Elixir not installed)", name);
            return Ok(());
        }

        println!("  Compiling {} (Elixir)...", name);

        // Create ebin directory
        fs::create_dir_all(&ebin_dir).map_err(|e| {
            DepsError::new(format!("Failed to create ebin for {}: {}", name, e))
        })?;

        // Collect all dependency ebin paths for -pa flags
        let dep_paths: Vec<PathBuf> = self.dep_ebin_paths();

        // Build elixirc command with all .ex files
        let mut cmd = std::process::Command::new("elixirc");
        cmd.arg("--ignore-module-conflict");
        cmd.arg("-o").arg(&ebin_dir);

        // Add dependency paths so Elixir can find already-compiled modules
        for dep_path in &dep_paths {
            cmd.arg("-pa").arg(dep_path);
        }

        // Add all .ex files
        for ex_file in &ex_files {
            cmd.arg(ex_file);
        }

        let status = cmd.status().map_err(|e| {
            DepsError::new(format!("Failed to run elixirc for {}: {}", name, e))
        })?;

        if !status.success() {
            return Err(DepsError::new(format!(
                "Failed to compile Elixir files in {}",
                name
            )));
        }

        println!("  {} compiled", name);
        Ok(())
    }

    /// Recursively find all .ex files in a directory.
    fn find_ex_files(dir: &Path) -> Vec<PathBuf> {
        let mut files = Vec::new();

        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    files.extend(Self::find_ex_files(&path));
                } else if path.extension().map_or(false, |ext| ext == "ex") {
                    files.push(path);
                }
            }
        }

        files
    }

    /// Get all ebin paths for dependencies.
    pub fn dep_ebin_paths(&self) -> Vec<PathBuf> {
        let deps_dir = self.deps_dir();
        let mut paths = Vec::new();

        if let Ok(entries) = fs::read_dir(&deps_dir) {
            for entry in entries.flatten() {
                let ebin = entry.path().join("ebin");
                if ebin.exists() {
                    paths.push(ebin);
                }
            }
        }

        paths
    }
}
