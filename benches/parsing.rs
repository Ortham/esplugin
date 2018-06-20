#[macro_use]
extern crate criterion;
extern crate esplugin;

use std::path::Path;

use criterion::Criterion;
use esplugin::{GameId, Plugin};

// Hearthfires.esm is a 3.8 MB file, so it's got plenty of content without being
// large enough to slow down benchmarking much.
const PLUGIN_TO_PARSE: &str = "testing-plugins/SkyrimSE/Data/Hearthfires.esm";

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("Plugin.parse_file() header-only", |b| {
        let mut plugin = Plugin::new(GameId::SkyrimSE, Path::new(PLUGIN_TO_PARSE));

        b.iter(|| {
            assert!(plugin.parse_file(true).is_ok());
        });
    });

    c.bench_function("Plugin.parse_file() full", |b| {
        let mut plugin = Plugin::new(GameId::SkyrimSE, Path::new(PLUGIN_TO_PARSE));

        b.iter(|| {
            assert!(plugin.parse_file(false).is_ok());
        });
    });

    c.bench_function("Plugin.overlaps_with()", |b| {
        let mut plugin = Plugin::new(GameId::SkyrimSE, Path::new(PLUGIN_TO_PARSE));

        assert!(plugin.parse_file(false).is_ok());

        b.iter(|| {
            assert!(plugin.overlaps_with(&plugin));
        });
    });

    c.bench_function("Plugin.count_override_records()", |b| {
        let mut plugin = Plugin::new(GameId::SkyrimSE, Path::new(PLUGIN_TO_PARSE));

        assert!(plugin.parse_file(false).is_ok());

        b.iter(|| {
            assert_eq!(plugin.count_override_records(), 1272);
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
