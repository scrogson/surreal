//! Project configuration for Dream projects.
//!
//! Handles parsing of `dream.toml` manifest files and project discovery.

use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

/// Error type for configuration operations.
#[derive(Debug)]
pub struct ConfigError {
    pub message: String,
}

impl ConfigError {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for ConfigError {}

/// Result type for configuration operations.
pub type ConfigResult<T> = Result<T, ConfigError>;

/// The parsed dream.toml configuration.
#[derive(Debug, Clone, Deserialize)]
pub struct ProjectConfig {
    pub package: Package,
}

/// Package metadata from dream.toml.
#[derive(Debug, Clone, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,
    #[serde(default = "default_src_dir")]
    pub src: String,
}

fn default_src_dir() -> String {
    "src".to_string()
}

impl ProjectConfig {
    /// Load configuration from a dream.toml file.
    pub fn load(path: &Path) -> ConfigResult<Self> {
        let content = fs::read_to_string(path).map_err(|e| {
            ConfigError::new(format!("Failed to read {}: {}", path.display(), e))
        })?;

        toml::from_str(&content).map_err(|e| {
            ConfigError::new(format!("Failed to parse {}: {}", path.display(), e))
        })
    }

    /// Find the project root by looking for dream.toml in current and parent directories.
    pub fn find_project_root() -> Option<PathBuf> {
        let mut current = std::env::current_dir().ok()?;

        loop {
            let config_path = current.join("dream.toml");
            if config_path.exists() {
                return Some(current);
            }

            if !current.pop() {
                return None;
            }
        }
    }

    /// Load configuration from the project root.
    pub fn from_project_root() -> ConfigResult<(PathBuf, Self)> {
        let root = Self::find_project_root().ok_or_else(|| {
            ConfigError::new("Could not find dream.toml in current or parent directories")
        })?;

        let config_path = root.join("dream.toml");
        let config = Self::load(&config_path)?;

        Ok((root, config))
    }

    /// Get the source directory path relative to project root.
    pub fn src_dir(&self, project_root: &Path) -> PathBuf {
        project_root.join(&self.package.src)
    }

    /// Get the build directory path relative to project root.
    pub fn build_dir(&self, project_root: &Path) -> PathBuf {
        project_root.join("_build")
    }

    /// Get the BEAM output directory.
    pub fn beam_dir(&self, project_root: &Path) -> PathBuf {
        self.build_dir(project_root).join("beam")
    }
}

/// Generate a default dream.toml content for a new project.
pub fn generate_dream_toml(name: &str) -> String {
    format!(
        r#"[package]
name = "{}"
version = "0.1.0"
"#,
        name
    )
}

/// Generate a default main.dream content for a new project.
pub fn generate_main_dream(name: &str) -> String {
    format!(
        r#"// {} - A Dream project

pub fn main() {{
    :ok
}}
"#,
        name
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_dream_toml() {
        let content = r#"
[package]
name = "test_project"
version = "0.1.0"
"#;
        let config: ProjectConfig = toml::from_str(content).unwrap();
        assert_eq!(config.package.name, "test_project");
        assert_eq!(config.package.version, "0.1.0");
        assert_eq!(config.package.src, "src");
    }

    #[test]
    fn test_parse_dream_toml_custom_src() {
        let content = r#"
[package]
name = "custom"
version = "1.0.0"
src = "lib"
"#;
        let config: ProjectConfig = toml::from_str(content).unwrap();
        assert_eq!(config.package.src, "lib");
    }

    #[test]
    fn test_generate_dream_toml() {
        let content = generate_dream_toml("my_app");
        assert!(content.contains("name = \"my_app\""));
        assert!(content.contains("version = \"0.1.0\""));
    }
}
