#[macro_use]
extern crate criterion;
extern crate esplugin;

use std::path::Path;

use criterion::Criterion;
use esplugin::{GameId, Plugin};

// Hearthfires.esm is a 3.8 MB file, so it's got plenty of content without being
// large enough to slow down benchmarking much.
const PLUGIN_TO_PARSE: &str =
    "C:\\Games\\Steam\\steamapps\\common\\Skyrim Special Edition\\Data\\Hearthfires.esm";

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
    c.bench_function("Plugin.parse_mmapped_file() header-only", |b| {
        let mut plugin = Plugin::new(GameId::SkyrimSE, Path::new(PLUGIN_TO_PARSE));

        b.iter(|| unsafe {
            assert!(plugin.parse_mmapped_file(true).is_ok());
        });
    });

    c.bench_function("Plugin.parse_mmapped_file() full", |b| {
        let mut plugin = Plugin::new(GameId::SkyrimSE, Path::new(PLUGIN_TO_PARSE));

        b.iter(|| unsafe {
            assert!(plugin.parse_mmapped_file(false).is_ok());
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
