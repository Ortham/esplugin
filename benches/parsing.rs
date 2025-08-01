#![allow(clippy::missing_assert_message, clippy::unwrap_used)]
use std::path::Path;

use criterion::Criterion;
use esplugin::{GameId, ParseOptions, Plugin};

// HearthFires.esm is a 3.8 MB file, so it's got plenty of content without being
// large enough to slow down benchmarking much.
// NOTE: This plugin isn't shipped in testing-plugins, it needs to be copied
// from Skyrim SE.
const PLUGIN_TO_PARSE: &str = "testing-plugins/SkyrimSE/Data/HearthFires.esm";
const GAME_ID: GameId = GameId::SkyrimSE;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("Plugin.parse_file() header-only", |b| {
        let mut plugin = Plugin::new(GAME_ID, Path::new(PLUGIN_TO_PARSE));

        b.iter(|| {
            plugin.parse_file(ParseOptions::header_only()).unwrap();
        });
    });

    c.bench_function("Plugin.parse_file() full", |b| {
        let mut plugin = Plugin::new(GAME_ID, Path::new(PLUGIN_TO_PARSE));

        b.iter(|| {
            plugin.parse_file(ParseOptions::whole_plugin()).unwrap();
        });
    });

    c.bench_function("Plugin.overlaps_with()", |b| {
        let mut plugin = Plugin::new(GAME_ID, Path::new(PLUGIN_TO_PARSE));

        plugin.parse_file(ParseOptions::whole_plugin()).unwrap();

        b.iter(|| {
            assert!(plugin.overlaps_with(&plugin).unwrap());
        });
    });

    c.bench_function("Plugin.count_override_records()", |b| {
        let mut plugin = Plugin::new(GAME_ID, Path::new(PLUGIN_TO_PARSE));

        plugin.parse_file(ParseOptions::whole_plugin()).unwrap();

        b.iter(|| {
            assert_eq!(plugin.count_override_records().unwrap(), 1272);
        });
    });

    c.bench_function("Plugin.is_master_file()", |b| {
        let mut plugin = Plugin::new(
            GAME_ID,
            Path::new("testing-plugins/SkyrimSE/Data/Blank.esp"),
        );
        plugin.parse_file(ParseOptions::header_only()).unwrap();

        b.iter(|| {
            assert!(!plugin.is_master_file());
        });
    });
}

criterion::criterion_group!(benches, criterion_benchmark);
criterion::criterion_main!(benches);
