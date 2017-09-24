#include <cassert>
#include <cstdio>
#include <cstring>

#include "libespm.hpp"

void test_game_id_values() {
  printf("testing ESPM_GAME_* values...\n");
  assert(ESPM_GAME_OBLIVION == 0);
  assert(ESPM_GAME_SKYRIM == 1);
  assert(ESPM_GAME_FALLOUT3 == 2);
  assert(ESPM_GAME_FALLOUTNV == 3);
  assert(ESPM_GAME_MORROWIND == 4);
  assert(ESPM_GAME_FALLOUT4 == 5);
}

void test_espm_plugin_new() {
  printf("testing espm_plugin_new()...\n");
  Plugin * plugin;
  auto return_code = espm_plugin_new(&plugin, ESPM_GAME_SKYRIM, "foo");
  assert(return_code == ESPM_OK);

  espm_plugin_free(plugin);
}

void test_espm_plugin_parse() {
  printf("testing espm_plugin_parse()...\n");
  Plugin * plugin;
  auto return_code = espm_plugin_new(&plugin, ESPM_GAME_SKYRIM, "../../testing-plugins/Skyrim/Data/Blank.esm");
  assert(return_code == ESPM_OK);

  return_code = espm_plugin_parse(plugin, true);
  assert(return_code == ESPM_OK);

  espm_plugin_free(plugin);
}

void test_espm_plugin_filename() {
  printf("testing espm_plugin_new()...\n");
  Plugin * plugin;
  auto return_code = espm_plugin_new(&plugin, ESPM_GAME_SKYRIM, "foo");
  assert(return_code == ESPM_OK);

  char * filename;
  return_code = espm_plugin_filename(plugin, &filename);
  assert(return_code == ESPM_OK);
  assert(strcmp(filename, "foo") == 0);

  espm_string_free(filename);
  espm_plugin_free(plugin);
}

void test_espm_plugin_masters() {
  printf("testing espm_plugin_masters()...\n");
  Plugin * plugin;
  auto return_code = espm_plugin_new(&plugin, ESPM_GAME_SKYRIM, "../../testing-plugins/Skyrim/Data/Blank - Master Dependent.esm");
  assert(return_code == ESPM_OK);

  return_code = espm_plugin_parse(plugin, true);
  assert(return_code == ESPM_OK);

  char ** masters;
  uint8_t num_masters;
  return_code = espm_plugin_masters(plugin, &masters, &num_masters);
  assert(return_code == ESPM_OK);
  assert(num_masters == 1);
  assert(strcmp(masters[0], "Blank.esm") == 0);

  espm_string_array_free(masters, num_masters);
  espm_plugin_free(plugin);
}

void test_espm_plugin_is_master() {
  printf("testing espm_plugin_is_master()...\n");
  Plugin * plugin;
  auto return_code = espm_plugin_new(&plugin, ESPM_GAME_SKYRIM, "../../testing-plugins/Skyrim/Data/Blank.esm");
  assert(return_code == ESPM_OK);

  return_code = espm_plugin_parse(plugin, true);
  assert(return_code == ESPM_OK);

  bool is_master;
  return_code = espm_plugin_is_master(plugin, &is_master);
  assert(return_code == ESPM_OK);
  assert(is_master);

  espm_plugin_free(plugin);
}

void test_espm_plugin_is_light_master() {
  printf("testing espm_plugin_is_light_master()...\n");
  Plugin * plugin;
  auto return_code = espm_plugin_new(&plugin, ESPM_GAME_FALLOUT4, "../../testing-plugins/Skyrim/Data/Blank.esl");
  assert(return_code == ESPM_OK);

  bool is_light_master;
  return_code = espm_plugin_is_light_master(plugin, &is_light_master);
  assert(return_code == ESPM_OK);
  assert(is_light_master);

  espm_plugin_free(plugin);
}

void test_espm_plugin_is_valid() {
  bool is_valid;
  auto return_code = espm_plugin_is_valid(ESPM_GAME_SKYRIM, "../../testing-plugins/Skyrim/Data/Blank.esm", true, &is_valid);
  assert(return_code == ESPM_OK);
  assert(is_valid);
}

void test_espm_plugin_description() {
  printf("testing espm_plugin_description()...\n");
  Plugin * plugin;
  auto return_code = espm_plugin_new(&plugin, ESPM_GAME_SKYRIM, "../../testing-plugins/Skyrim/Data/Blank.esm");
  assert(return_code == ESPM_OK);

  return_code = espm_plugin_parse(plugin, true);
  assert(return_code == ESPM_OK);

  char * description;
  return_code = espm_plugin_description(plugin, &description);
  assert(return_code == ESPM_OK);
  assert(strcmp(description, "v5.0") == 0);

  espm_string_free(description);
  espm_plugin_free(plugin);
}

void test_espm_plugin_is_empty() {
  printf("testing espm_plugin_is_empty()...\n");
  Plugin * plugin;
  auto return_code = espm_plugin_new(&plugin, ESPM_GAME_SKYRIM, "../../testing-plugins/Skyrim/Data/Blank.esm");
  assert(return_code == ESPM_OK);

  return_code = espm_plugin_parse(plugin, true);
  assert(return_code == ESPM_OK);

  bool is_empty;
  return_code = espm_plugin_is_empty(plugin, &is_empty);
  assert(return_code == ESPM_OK);
  assert(!is_empty);

  espm_plugin_free(plugin);
}

void test_espm_plugin_count_override_records() {
  printf("testing espm_plugin_count_override_records()...\n");
  Plugin * plugin;
  auto return_code = espm_plugin_new(&plugin, ESPM_GAME_SKYRIM, "../../testing-plugins/Skyrim/Data/Blank.esm");
  assert(return_code == ESPM_OK);

  return_code = espm_plugin_parse(plugin, true);
  assert(return_code == ESPM_OK);

  size_t count;
  return_code = espm_plugin_count_override_records(plugin, &count);
  assert(return_code == ESPM_OK);
  assert(count == 0);

  espm_plugin_free(plugin);
}

void test_espm_plugin_do_records_overlap() {
  printf("testing espm_plugin_do_records_overlap()...\n");
  Plugin * plugin;
  auto return_code = espm_plugin_new(&plugin, ESPM_GAME_SKYRIM, "../../testing-plugins/Skyrim/Data/Blank.esm");
  assert(return_code == ESPM_OK);

  return_code = espm_plugin_parse(plugin, false);
  assert(return_code == ESPM_OK);

  bool overlap;
  return_code = espm_plugin_do_records_overlap(plugin, plugin, &overlap);
  assert(return_code == ESPM_OK);
  assert(overlap);

  espm_plugin_free(plugin);
}

int main() {
  test_game_id_values();

  test_espm_plugin_new();
  test_espm_plugin_parse();
  test_espm_plugin_filename();
  test_espm_plugin_masters();
  test_espm_plugin_is_master();
  test_espm_plugin_is_light_master();
  test_espm_plugin_is_valid();
  test_espm_plugin_description();
  test_espm_plugin_is_empty();
  test_espm_plugin_count_override_records();
  test_espm_plugin_do_records_overlap();

  printf("SUCCESS\n");
  return 0;
}
