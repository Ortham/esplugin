#!/usr/bin/env python

import ctypes
from ctypes import c_char_p, c_uint, Structure, POINTER, pointer, byref
import os.path
import sys
import unittest

def load_library(path):
    prefix = {u'win32' : u''}.get(sys.platform, u'lib')
    extension = {u'darwin': u'.dylib', u'win32': u'.dll'}.get(sys.platform, u'.so')

    lib_path = os.path.join(path, prefix + u'espm' + extension)
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

class FormId(Structure):
    _fields_ = [
        ('object_index', c_uint),
    ]
    pass

class GameIdTest(unittest.TestCase):
    def test_game_id_values(self):
        self.assertEqual(0, get_constant(lib, "ESPM_GAME_OBLIVION"))
        self.assertEqual(1, get_constant(lib, "ESPM_GAME_SKYRIM"))
        self.assertEqual(2, get_constant(lib, "ESPM_GAME_FALLOUT3"))
        self.assertEqual(3, get_constant(lib, "ESPM_GAME_FALLOUTNV"))
        self.assertEqual(4, get_constant(lib, "ESPM_GAME_MORROWIND"))
        self.assertEqual(5, get_constant(lib, "ESPM_GAME_FALLOUT4"))

class FormIdTest(unittest.TestCase):
    def setUp(self):
        self.OK = get_constant(lib, "ESPM_OK")
        self.formid = pointer(FormId())
        self.name = c_char_p()

    def tearDown(self):
        lib.espm_formid_free(self.formid)
        lib.espm_string_free(self.name)

    def test_creating_a_new_formid_object(self):
        masters = (c_char_p * 0)()
        masters[:] = []
        ret = lib.espm_formid_new(
            byref(self.formid),
            "foo".encode('utf-8'),
            masters,
            0,
            1)
        self.assertEqual(self.OK, ret)
        self.assertEqual(1, self.formid[0].object_index)

    def test_getting_a_formid_plugin_name(self):
        masters = (c_char_p * 0)()
        masters[:] = []
        ret = lib.espm_formid_new(
            byref(self.formid),
            "foo".encode('utf-8'),
            masters,
            0,
            1)
        self.assertEqual(self.OK, ret)
        self.assertEqual(1, self.formid[0].object_index)

        ret = lib.espm_formid_plugin_name(byref(self.name), self.formid)
        self.assertEqual(self.OK, ret)
        self.assertEqual(b'foo', self.name.value)

lib = load_library(os.path.join('target', 'debug'))

if __name__ == '__main__':
    unittest.main()
