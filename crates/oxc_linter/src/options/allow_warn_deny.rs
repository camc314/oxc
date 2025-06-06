use std::{
    borrow::Cow,
    convert::From,
    fmt::{self, Display},
};

use schemars::{JsonSchema, schema::SchemaObject};
use serde::{Deserialize, Serialize, de};
use serde_json::{Number, Value};

use oxc_diagnostics::{OxcDiagnostic, Severity};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum AllowWarnDeny {
    Allow, // Off
    Warn,  // Warn
    Deny,  // Error
}

impl AllowWarnDeny {
    pub fn is_warn_deny(self) -> bool {
        self != Self::Allow
    }

    pub fn is_allow(self) -> bool {
        self == Self::Allow
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Allow => "allow",
            Self::Warn => "warn",
            Self::Deny => "deny",
        }
    }
}

impl fmt::Display for AllowWarnDeny {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl TryFrom<&str> for AllowWarnDeny {
    type Error = OxcDiagnostic;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "allow" | "off" => Ok(Self::Allow),
            "deny" | "error" => Ok(Self::Deny),
            "warn" => Ok(Self::Warn),
            _ => Err(OxcDiagnostic::error(format!(
                r#"Failed to parse rule severity, expected one of "allow", "off", "deny", "error" or "warn", but got {s:?}"#
            ))),
        }
    }
}

impl TryFrom<&Value> for AllowWarnDeny {
    type Error = OxcDiagnostic;

    fn try_from(value: &Value) -> Result<Self, OxcDiagnostic> {
        match value {
            Value::String(s) => Self::try_from(s.as_str()),
            Value::Number(n) => Self::try_from(n),
            _ => Err(OxcDiagnostic::error(format!(
                "Failed to parse rule severity, expected a string or a number, but got {value:?}"
            ))),
        }
    }
}

fn invalid_int_severity<D: Display>(value: D) -> OxcDiagnostic {
    OxcDiagnostic::error(format!(
        r"Failed to parse rule severity, expected one of `0`, `1` or `2`, but got {value}"
    ))
}

impl TryFrom<u64> for AllowWarnDeny {
    type Error = OxcDiagnostic;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Allow),
            1 => Ok(Self::Warn),
            2 => Ok(Self::Deny),
            x => Err(invalid_int_severity(x)),
        }
    }
}

impl TryFrom<i64> for AllowWarnDeny {
    type Error = OxcDiagnostic;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        if value < 0 {
            return Err(invalid_int_severity("a negative number"));
        }
        #[expect(clippy::cast_sign_loss)]
        Self::try_from(value as u64)
    }
}

impl TryFrom<&Number> for AllowWarnDeny {
    type Error = OxcDiagnostic;

    fn try_from(value: &Number) -> Result<Self, Self::Error> {
        let value = value.as_i64().ok_or_else(|| invalid_int_severity(value))?;
        Self::try_from(value)
    }
}

impl<'de> Deserialize<'de> for AllowWarnDeny {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: de::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum Repr<'a> {
            Int(u8),
            Str(Cow<'a, str>),
        }

        match Repr::deserialize(deserializer)? {
            Repr::Int(0) => Ok(Self::Allow),
            Repr::Int(1) => Ok(Self::Warn),
            Repr::Int(2) => Ok(Self::Deny),
            #[expect(clippy::cast_lossless)]
            Repr::Int(other) => Err(de::Error::invalid_value(
                de::Unexpected::Unsigned(other as u64),
                &"`0`, `1` or `2`",
            )),
            Repr::Str(s) => match &*s {
                "allow" | "off" => Ok(Self::Allow),
                "warn" => Ok(Self::Warn),
                "deny" | "error" => Ok(Self::Deny),
                _ => Err(de::Error::invalid_value(
                    de::Unexpected::Str(&s),
                    &"`allow`, `off`, `warn`, `deny` or `error`",
                )),
            },
        }
    }
}

impl JsonSchema for AllowWarnDeny {
    fn schema_name() -> String {
        "AllowWarnDeny".to_string()
    }

    fn schema_id() -> std::borrow::Cow<'static, str> {
        "AllowWarnDeny".into()
    }

    fn json_schema(r#gen: &mut schemars::r#gen::SchemaGenerator) -> schemars::schema::Schema {
        let mut string_schema = <String as JsonSchema>::json_schema(r#gen).into_object();
        string_schema.enum_values =
            Some(vec!["allow".into(), "off".into(), "warn".into(), "error".into(), "deny".into()]);
        string_schema.metadata().description = Some(
            r#"Oxlint rule.
- "allow" or "off": Turn off the rule.
- "warn": Turn the rule on as a warning (doesn't affect exit code).
- "error" or "deny": Turn the rule on as an error (will exit with a failure code)."#
                .to_string(),
        );
        let mut int_schema = <u32 as JsonSchema>::json_schema(r#gen).into_object();
        int_schema.number().minimum = Some(0.0);
        int_schema.number().maximum = Some(2.0);
        int_schema.metadata().description = Some(
            "Oxlint rule.
    
- 0: Turn off the rule.
- 1: Turn the rule on as a warning (doesn't affect exit code).
- 2: Turn the rule on as an error (will exit with a failure code)."
                .to_string(),
        );

        let mut schema = SchemaObject::default();
        schema.subschemas().one_of = Some(vec![string_schema.into(), int_schema.into()]);

        schema.into()
    }
}

impl From<AllowWarnDeny> for Severity {
    fn from(value: AllowWarnDeny) -> Self {
        match value {
            AllowWarnDeny::Allow => Self::Advice,
            AllowWarnDeny::Warn => Self::Warning,
            AllowWarnDeny::Deny => Self::Error,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_serialize() {
        let tests = [
            (AllowWarnDeny::Allow, r#""allow""#),
            (AllowWarnDeny::Warn, r#""warn""#),
            (AllowWarnDeny::Deny, r#""deny""#),
        ];
        for (input, expected) in tests {
            assert_eq!(serde_json::to_string(&input).unwrap(), expected);
        }
    }

    #[test]
    fn test_deserialize() {
        let pass = [
            // allow
            (r#""allow""#, AllowWarnDeny::Allow),
            (r#""off""#, AllowWarnDeny::Allow),
            ("0", AllowWarnDeny::Allow),
            // warn
            (r#""warn""#, AllowWarnDeny::Warn),
            ("1", AllowWarnDeny::Warn),
            // deny
            (r#""error""#, AllowWarnDeny::Deny),
            (r#""deny""#, AllowWarnDeny::Deny),
            ("2", AllowWarnDeny::Deny),
        ];

        for (input, expected) in pass {
            let msg = format!("input: {input}");
            let actual: AllowWarnDeny = serde_json::from_str(input).expect(&msg);
            assert_eq!(actual, expected);
        }

        let fail = [r#""foo""#, "-1", "3"];
        for input in fail {
            assert!(serde_json::from_str::<AllowWarnDeny>(input).is_err());
        }
    }
}
