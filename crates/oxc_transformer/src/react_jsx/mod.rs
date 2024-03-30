use serde::Deserialize;

#[derive(Debug, Default, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReactJsxOptions;

/// [plugin-transform-react-jsx](https://babeljs.io/docs/babel-plugin-transform-react-jsx)
///
/// This plugin generates production-ready JS code.
///
/// If you are developing a React app in a development environment,
/// please use @babel/plugin-transform-react-jsx-development for a better debugging experience.
///
/// This plugin is included in `preset-react`.
#[derive(Debug, Default)]
pub struct ReactJsx {
    #[allow(unused)]
    options: ReactJsxOptions,
}

impl ReactJsx {
    pub fn new(options: ReactJsxOptions) -> Self {
        Self { options }
    }
}
