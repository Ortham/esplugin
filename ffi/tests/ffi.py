#!/usr/bin/env python

import ctypes
from ctypes import c_char_p, c_uint, c_char, c_bool, c_size_t, Structure, POINTER, pointer, byref
import os.path
import sys
import unittest

def load_library(path):
    prefix = {u'win32' : u''}.get(sys.platform, u'lib')
    extension = {u'darwin': u'.dylib', u'win32': u'.dll'}.get(sys.platform, u'.so')

    lib_path = os.path.join(path, prefix + u'espm_ffi' + extension)
    return ctypes.cdll.LoadLibrary(lib_path)

def get_constant(lib, name):
    return ctypes.c_uint.in_dll(lib, name).value

def set_function_types(lib):
    lib.espm_formid_new.restype = c_uint
    lib.espm_formid_new.argtypes = (
        POINTER(POINTER(FormId)),
        c_char_p,
        POINTER(c_char_p),
        c_uint,
        c_uint,
    )

    lib.espm_formid_free.argtypes = (POINTER(FormId), )

    lib.espm_formid_plugin_name.restype = c_uint
    lib.espm_formid_plugin_name.argtypes = (
        POINTER(c_char_p),
        POINTER(FormId),
    )

    lib.espm_string_free.argtypes = (c_char_p, )
    lib.espm_string_array_free.argtypes = (POINTER(c_char_p), c_uint)

    lib.espm_plugin_new.restype = c_uint
    lib.espm_plugin_new.argtypes = (
        POINTER(POINTER(Plugin)),
        c_uint,
        c_char_p,
    )

    lib.espm_plugin_free.argtypes = (POINTER(Plugin), )

    lib.espm_plugin_filename.restype = c_uint
    lib.espm_plugin_filename.argtypes = (
        POINTER(c_char_p),
        POINTER(Plugin),
    )

    lib.espm_plugin_parse.restype = c_uint
    lib.espm_plugin_parse.argtypes = (
        POINTER(Plugin),
        c_bool,
    )

    lib.espm_plugin_masters.restype = c_uint
    lib.espm_plugin_masters.argtypes = (
        POINTER(Plugin),
        POINTER(POINTER(c_char_p)),
        POINTER(c_uint),
    )

    lib.espm_plugin_is_master.restype = c_uint
    lib.espm_plugin_is_master.argtypes = (
        POINTER(Plugin),
        POINTER(c_bool)
    )

    lib.espm_plugin_is_light_master.restype = c_uint
    lib.espm_plugin_is_light_master.argtypes = (
        POINTER(Plugin),
        POINTER(c_bool)
    )

    lib.espm_plugin_is_valid.restype = c_uint
    lib.espm_plugin_is_valid.argtypes = (
        c_uint,
        c_char_p,
        c_bool,
        POINTER(c_bool)
    )

    lib.espm_plugin_description.restype = c_uint
    lib.espm_plugin_description.argtypes = (
        POINTER(c_char_p),
        POINTER(Plugin),
    )

    lib.espm_plugin_is_empty.restype = c_uint
    lib.espm_plugin_is_empty.argtypes = (
        POINTER(Plugin),
        POINTER(bool)
    )

    lib.espm_plugin_count_override_records.restype = c_uint
    lib.espm_plugin_count_override_records.argtypes = (
        POINTER(Plugin),
        POINTER(c_size_t)
    )

    lib.espm_plugin_do_records_overlap.restype = c_uint
    lib.espm_plugin_do_records_overlap.argtypes = (
        POINTER(Plugin),
        POINTER(Plugin),
        POINTER(c_bool)
    )

class Plugin(Structure):
    pass

class GameIdTest(unittest.TestCase):
    def test_game_id_values(self):
        self.assertEqual(0, get_constant(lib, "ESPM_GAME_OBLIVION"))
        self.assertEqual(1, get_constant(lib, "ESPM_GAME_SKYRIM"))
        self.assertEqual(2, get_constant(lib, "ESPM_GAME_FALLOUT3"))
        self.assertEqual(3, get_constant(lib, "ESPM_GAME_FALLOUTNV"))
        self.assertEqual(4, get_constant(lib, "ESPM_GAME_MORROWIND"))
        self.assertEqual(5, get_constant(lib, "ESPM_GAME_FALLOUT4"))

class PluginTest(unittest.TestCase):
    def setUp(self):
        self.OK = get_constant(lib, "ESPM_OK")
        self.plugin = pointer(Plugin())
        self.filename = c_char_p()
        self.masters = pointer(c_char_p())
        self.masters_size = c_uint(0)
        self.description = c_char_p()

    def tearDown(self):
        lib.espm_plugin_free(self.plugin)
        lib.espm_string_free(self.filename)
        lib.espm_string_array_free(self.masters, self.masters_size)
        lib.espm_string_free(self.description)

    def test_creating_a_new_plugin_object(self):
        ret = lib.espm_plugin_new(
            byref(self.plugin),
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'foo'.encode('utf-8'))
        self.assertEqual(self.OK, ret)

    def test_parsing_a_plugin(self):
        ret = lib.espm_plugin_new(
            byref(self.plugin),
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'testing-plugins/Skyrim/Data/Blank.esm'.encode('utf-8'))
        self.assertEqual(self.OK, ret)

        ret = lib.espm_plugin_parse(self.plugin, True)
        self.assertEqual(self.OK, ret)

    def test_getting_a_plugin_object_filename(self):
        ret = lib.espm_plugin_new(
            byref(self.plugin),
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'foo'.encode('utf-8'))
        self.assertEqual(self.OK, ret)

        ret = lib.espm_plugin_filename(self.plugin, byref(self.filename))
        self.assertEqual(self.OK, ret)
        self.assertEqual(b'foo', self.filename.value)

    def test_getting_a_plugins_masters(self):
        ret = lib.espm_plugin_new(
            byref(self.plugin),
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'testing-plugins/Skyrim/Data/Blank - Master Dependent.esm'.encode('utf-8'))
        self.assertEqual(self.OK, ret)

        ret = lib.espm_plugin_parse(self.plugin, True)
        self.assertEqual(self.OK, ret)

        ret = lib.espm_plugin_masters(self.plugin, byref(self.masters), byref(self.masters_size))
        self.assertEqual(self.OK, ret)
        self.assertEqual(1, self.masters_size.value)
        self.assertEqual(b'Blank.esm', self.masters[0])

    def test_checking_if_a_plugin_is_a_master_file(self):
        ret = lib.espm_plugin_new(
            byref(self.plugin),
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'testing-plugins/Skyrim/Data/Blank.esm'.encode('utf-8'))
        self.assertEqual(self.OK, ret)

        ret = lib.espm_plugin_parse(self.plugin, True)
        self.assertEqual(self.OK, ret)

        is_master = c_bool()
        ret = lib.espm_plugin_is_master(self.plugin, byref(is_master))
        self.assertEqual(self.OK, ret)
        self.assertTrue(is_master.value)

    def test_checking_if_a_plugin_is_a_light_master_file(self):
        ret = lib.espm_plugin_new(
            byref(self.plugin),
            get_constant(lib, 'ESPM_GAME_FALLOUT4'),
            'testing-plugins/Skyrim/Data/Blank.esl'.encode('utf-8'))

        is_master = c_bool()
        ret = lib.espm_plugin_is_light_master(self.plugin, byref(is_master))
        self.assertEqual(self.OK, ret)
        self.assertTrue(is_master.value)

    def test_checking_if_a_plugin_is_valid(self):
        self.plugin = None

        is_valid = c_bool()
        ret = lib.espm_plugin_is_valid(
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'testing-plugins/Skyrim/Data/Blank.esm'.encode('utf-8'),
            True,
            byref(is_valid))
        self.assertEqual(self.OK, ret)
        self.assertTrue(is_valid.value)

        ret = lib.espm_plugin_is_valid(
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'README.md'.encode('utf-8'),
            True,
            byref(is_valid))
        self.assertEqual(self.OK, ret)
        self.assertFalse(is_valid.value)

    def test_getting_a_plugin_description(self):
        ret = lib.espm_plugin_new(
            byref(self.plugin),
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'testing-plugins/Skyrim/Data/Blank.esm'.encode('utf-8'))
        self.assertEqual(self.OK, ret)

        ret = lib.espm_plugin_parse(self.plugin, True)
        self.assertEqual(self.OK, ret)

        is_master = c_bool()
        ret = lib.espm_plugin_description(self.plugin, byref(self.description))
        self.assertEqual(self.OK, ret)
        self.assertEqual(b'v5.0', self.description.value)

    def test_checking_if_a_plugin_is_empty(self):
        ret = lib.espm_plugin_new(
            byref(self.plugin),
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'testing-plugins/Skyrim/Data/Blank.esm'.encode('utf-8'))
        self.assertEqual(self.OK, ret)

        ret = lib.espm_plugin_parse(self.plugin, True)
        self.assertEqual(self.OK, ret)

        is_empty = c_bool()
        ret = lib.espm_plugin_is_empty(self.plugin, byref(is_empty))
        self.assertEqual(self.OK, ret)
        self.assertFalse(is_empty.value)

    def test_counting_a_plugins_overlap_records(self):
        ret = lib.espm_plugin_new(
            byref(self.plugin),
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'testing-plugins/Skyrim/Data/Blank.esm'.encode('utf-8'))
        self.assertEqual(self.OK, ret)

        ret = lib.espm_plugin_parse(self.plugin, False)
        self.assertEqual(self.OK, ret)

        count = c_size_t()
        ret = lib.espm_plugin_count_override_records(self.plugin, byref(count))
        self.assertEqual(self.OK, ret)

        self.assertEqual(0, count.value)

    def test_checking_if_two_plugins_overlap(self):
        ret = lib.espm_plugin_new(
            byref(self.plugin),
            get_constant(lib, 'ESPM_GAME_SKYRIM'),
            'testing-plugins/Skyrim/Data/Blank.esm'.encode('utf-8'))
        self.assertEqual(self.OK, ret)

        ret = lib.espm_plugin_parse(self.plugin, False)
        self.assertEqual(self.OK, ret)

        do_overlap = c_bool()
        ret = lib.espm_plugin_do_records_overlap(self.plugin, self.plugin, byref(do_overlap))
        self.assertEqual(self.OK, ret)

        self.assertTrue(do_overlap.value)



lib = load_library(os.path.join('target', 'debug'))

if __name__ == '__main__':
    unittest.main()
