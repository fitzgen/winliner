#![cfg(feature = "serde")]

use anyhow::Result;
use winliner::{Profile, ProfileBuilder};

fn assert_profile(profile: Profile, expected: &str) -> Result<()> {
    let expected = expected.trim();
    println!("Expected profile:\n{expected}");

    let actual = serde_json::to_string_pretty(&profile)?;
    let actual = actual.trim();
    println!("Actual profile:\n{actual}");

    assert_eq!(expected, actual);
    Ok(())
}

#[test]
fn basic() -> Result<()> {
    let mut builder = ProfileBuilder::new();

    for _ in 0..10 {
        builder.add_indirect_call(42, 36);
    }

    for _ in 0..3 {
        builder.add_indirect_call(666, 36);
    }

    builder.add_indirect_call(123, 456);

    let profile = builder.build();

    assert_profile(
        profile,
        r#"
{
  "call_sites": {
    "36": {
      "total_call_count": 13,
      "callee_to_count": {
        "42": 10,
        "666": 3
      }
    },
    "456": {
      "total_call_count": 1,
      "callee_to_count": {
        "123": 1
      }
    }
  }
}
        "#,
    )
}

#[test]
fn merge() -> Result<()> {
    let mut profile1 = {
        let mut builder = ProfileBuilder::new();
        builder.add_indirect_call(123, 456);
        builder.add_indirect_call(456, 789);
        builder.build()
    };
    let profile2 = {
        let mut builder = ProfileBuilder::new();
        builder.add_indirect_call(123, 456);
        builder.add_indirect_call(321, 987);
        builder.build()
    };

    profile1.merge(&profile2);

    assert_profile(
        profile1,
        r#"
{
  "call_sites": {
    "456": {
      "total_call_count": 2,
      "callee_to_count": {
        "123": 2
      }
    },
    "789": {
      "total_call_count": 1,
      "callee_to_count": {
        "456": 1
      }
    },
    "987": {
      "total_call_count": 1,
      "callee_to_count": {
        "321": 1
      }
    }
  }
}
        "#,
    )
}
