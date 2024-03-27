/* Invisible Vector Library
 * coded by Ketmar // Invisible Vector <ketmar@ketmar.no-ip.org>
 * Understanding is not required. Only obedience.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
module iv.vfs.koi8 /*is aliced*/;
private:
import iv.alice;


// ////////////////////////////////////////////////////////////////////////// //
public string koi8lotranslit (string s) {
  usize pos = 0;
  while (pos < s.length && s.ptr[pos] < 0x80) ++pos;
  if (pos >= s.length) return s; // nothing to do
  string res = s[0..pos];
  while (pos < s.length) {
    char ch = s.ptr[pos++];
         if (ch == '\xe1' || ch == '\xc1') res ~= "a";
    else if (ch == '\xe2' || ch == '\xc2') res ~= "b";
    else if (ch == '\xf7' || ch == '\xd7') res ~= "v";
    else if (ch == '\xe7' || ch == '\xc7') res ~= "g";
    else if (ch == '\xe4' || ch == '\xc4') res ~= "d";
    else if (ch == '\xe5' || ch == '\xc5') res ~= "e";
    else if (ch == '\xb3' || ch == '\xa3') res ~= "yo";
    else if (ch == '\xf6' || ch == '\xd6') res ~= "zh";
    else if (ch == '\xfa' || ch == '\xda') res ~= "z";
    else if (ch == '\xe9' || ch == '\xc9') res ~= "i";
    else if (ch == '\xea' || ch == '\xca') res ~= "j";
    else if (ch == '\xeb' || ch == '\xcb') res ~= "k";
    else if (ch == '\xec' || ch == '\xcc') res ~= "l";
    else if (ch == '\xed' || ch == '\xcd') res ~= "m";
    else if (ch == '\xee' || ch == '\xce') res ~= "n";
    else if (ch == '\xef' || ch == '\xcf') res ~= "o";
    else if (ch == '\xf0' || ch == '\xd0') res ~= "p";
    else if (ch == '\xf2' || ch == '\xd2') res ~= "r";
    else if (ch == '\xf3' || ch == '\xd3') res ~= "s";
    else if (ch == '\xf4' || ch == '\xd4') res ~= "t";
    else if (ch == '\xf5' || ch == '\xd5') res ~= "u";
    else if (ch == '\xe6' || ch == '\xc6') res ~= "f";
    else if (ch == '\xe8' || ch == '\xc8') res ~= "h";
    else if (ch == '\xe3' || ch == '\xc3') res ~= "c";
    else if (ch == '\xfe' || ch == '\xde') res ~= "ch";
    else if (ch == '\xfb' || ch == '\xdb') res ~= "sh";
    else if (ch == '\xfd' || ch == '\xdd') res ~= "sch";
    else if (ch == '\xff' || ch == '\xdf') res ~= "x";
    else if (ch == '\xf9' || ch == '\xd9') res ~= "y";
    else if (ch == '\xf8' || ch == '\xd8') res ~= "w";
    else if (ch == '\xfc' || ch == '\xdc') res ~= "e";
    else if (ch == '\xe0' || ch == '\xc0') res ~= "ju";
    else if (ch == '\xf1' || ch == '\xd1') res ~= "ja";
    else res ~= ch;
  }
  return res;
}


// ////////////////////////////////////////////////////////////////////////// //
public immutable char[256] koi8from866Table = [
  '\x00','\x01','\x02','\x03','\x04','\x05','\x06','\x07','\x08','\x09','\x0a','\x0b','\x0c','\x0d','\x0e','\x0f',
  '\x10','\x11','\x12','\x13','\x14','\x15','\x16','\x17','\x18','\x19','\x1a','\x1b','\x1c','\x1d','\x1e','\x1f',
  '\x20','\x21','\x22','\x23','\x24','\x25','\x26','\x27','\x28','\x29','\x2a','\x2b','\x2c','\x2d','\x2e','\x2f',
  '\x30','\x31','\x32','\x33','\x34','\x35','\x36','\x37','\x38','\x39','\x3a','\x3b','\x3c','\x3d','\x3e','\x3f',
  '\x40','\x41','\x42','\x43','\x44','\x45','\x46','\x47','\x48','\x49','\x4a','\x4b','\x4c','\x4d','\x4e','\x4f',
  '\x50','\x51','\x52','\x53','\x54','\x55','\x56','\x57','\x58','\x59','\x5a','\x5b','\x5c','\x5d','\x5e','\x5f',
  '\x60','\x61','\x62','\x63','\x64','\x65','\x66','\x67','\x68','\x69','\x6a','\x6b','\x6c','\x6d','\x6e','\x6f',
  '\x70','\x71','\x72','\x73','\x74','\x75','\x76','\x77','\x78','\x79','\x7a','\x7b','\x7c','\x7d','\x7e','\x7f',
  '\xe1','\xe2','\xf7','\xe7','\xe4','\xe5','\xf6','\xfa','\xe9','\xea','\xeb','\xec','\xed','\xee','\xef','\xf0',
  '\xf2','\xf3','\xf4','\xf5','\xe6','\xe8','\xe3','\xfe','\xfb','\xfd','\xff','\xf9','\xf8','\xfc','\xe0','\xf1',
  '\xc1','\xc2','\xd7','\xc7','\xc4','\xc5','\xd6','\xda','\xc9','\xca','\xcb','\xcc','\xcd','\xce','\xcf','\xd0',
  '\x90','\x91','\x92','\x81','\x87','\xb2','\x3f','\x3f','\x3f','\xb5','\xa1','\xa8','\xae','\x3f','\xac','\x83',
  '\x84','\x89','\x88','\x86','\x80','\x8a','\xaf','\xb0','\xab','\xa5','\xbb','\xb8','\xb1','\xa0','\xbe','\xb9',
  '\xba','\x3f','\x3f','\xaa','\xa9','\xa2','\x3f','\x3f','\xbc','\x85','\x82','\x8d','\x8c','\x8e','\x8f','\x8b',
  '\xd2','\xd3','\xd4','\xd5','\xc6','\xc8','\xc3','\xde','\xdb','\xdd','\xdf','\xd9','\xd8','\xdc','\xc0','\xd1',
  '\xb3','\xa3','\xb4','\xa4','\xb7','\xa7','\x3f','\x3f','\x9c','\x95','\x9e','\x96','\x3f','\x3f','\x94','\x9a',
];

public immutable char[256] koi8from1251Table = [
  '\x00','\x01','\x02','\x03','\x04','\x05','\x06','\x07','\x08','\x09','\x0a','\x0b','\x0c','\x0d','\x0e','\x0f',
  '\x10','\x11','\x12','\x13','\x14','\x15','\x16','\x17','\x18','\x19','\x1a','\x1b','\x1c','\x1d','\x1e','\x1f',
  '\x20','\x21','\x22','\x23','\x24','\x25','\x26','\x27','\x28','\x29','\x2a','\x2b','\x2c','\x2d','\x2e','\x2f',
  '\x30','\x31','\x32','\x33','\x34','\x35','\x36','\x37','\x38','\x39','\x3a','\x3b','\x3c','\x3d','\x3e','\x3f',
  '\x40','\x41','\x42','\x43','\x44','\x45','\x46','\x47','\x48','\x49','\x4a','\x4b','\x4c','\x4d','\x4e','\x4f',
  '\x50','\x51','\x52','\x53','\x54','\x55','\x56','\x57','\x58','\x59','\x5a','\x5b','\x5c','\x5d','\x5e','\x5f',
  '\x60','\x61','\x62','\x63','\x64','\x65','\x66','\x67','\x68','\x69','\x6a','\x6b','\x6c','\x6d','\x6e','\x6f',
  '\x70','\x71','\x72','\x73','\x74','\x75','\x76','\x77','\x78','\x79','\x7a','\x7b','\x7c','\x7d','\x7e','\x7f',
  '\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f',
  '\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f','\x3f',
  '\x9a','\x3f','\x3f','\x3f','\x3f','\xbd','\x3f','\x3f','\xb3','\xbf','\xb4','\x3f','\x3f','\x3f','\x3f','\xb7',
  '\x9c','\x3f','\xb6','\xa6','\xad','\x3f','\x3f','\x9e','\xa3','\x3f','\xa4','\x3f','\x3f','\x3f','\x3f','\xa7',
  '\xe1','\xe2','\xf7','\xe7','\xe4','\xe5','\xf6','\xfa','\xe9','\xea','\xeb','\xec','\xed','\xee','\xef','\xf0',
  '\xf2','\xf3','\xf4','\xf5','\xe6','\xe8','\xe3','\xfe','\xfb','\xfd','\xff','\xf9','\xf8','\xfc','\xe0','\xf1',
  '\xc1','\xc2','\xd7','\xc7','\xc4','\xc5','\xd6','\xda','\xc9','\xca','\xcb','\xcc','\xcd','\xce','\xcf','\xd0',
  '\xd2','\xd3','\xd4','\xd5','\xc6','\xc8','\xc3','\xde','\xdb','\xdd','\xdf','\xd9','\xd8','\xdc','\xc0','\xd1',
];

// char toupper/tolower, koi8
public immutable char[256] koi8tolowerTable = [
  '\x00','\x01','\x02','\x03','\x04','\x05','\x06','\x07','\x08','\x09','\x0a','\x0b','\x0c','\x0d','\x0e','\x0f',
  '\x10','\x11','\x12','\x13','\x14','\x15','\x16','\x17','\x18','\x19','\x1a','\x1b','\x1c','\x1d','\x1e','\x1f',
  '\x20','\x21','\x22','\x23','\x24','\x25','\x26','\x27','\x28','\x29','\x2a','\x2b','\x2c','\x2d','\x2e','\x2f',
  '\x30','\x31','\x32','\x33','\x34','\x35','\x36','\x37','\x38','\x39','\x3a','\x3b','\x3c','\x3d','\x3e','\x3f',
  '\x40','\x61','\x62','\x63','\x64','\x65','\x66','\x67','\x68','\x69','\x6a','\x6b','\x6c','\x6d','\x6e','\x6f',
  '\x70','\x71','\x72','\x73','\x74','\x75','\x76','\x77','\x78','\x79','\x7a','\x5b','\x5c','\x5d','\x5e','\x5f',
  '\x60','\x61','\x62','\x63','\x64','\x65','\x66','\x67','\x68','\x69','\x6a','\x6b','\x6c','\x6d','\x6e','\x6f',
  '\x70','\x71','\x72','\x73','\x74','\x75','\x76','\x77','\x78','\x79','\x7a','\x7b','\x7c','\x7d','\x7e','\x7f',
  '\x80','\x81','\x82','\x83','\x84','\x85','\x86','\x87','\x88','\x89','\x8a','\x8b','\x8c','\x8d','\x8e','\x8f',
  '\x90','\x91','\x92','\x93','\x94','\x95','\x96','\x97','\x98','\x99','\x9a','\x9b','\x9c','\x9d','\x9e','\x9f',
  '\xa0','\xa1','\xa2','\xa3','\xa4','\xa5','\xa6','\xa7','\xa8','\xa9','\xaa','\xab','\xac','\xad','\xae','\xaf',
  '\xb0','\xb1','\xb2','\xa3','\xa4','\xb5','\xa6','\xa7','\xb8','\xb9','\xba','\xbb','\xbc','\xad','\xbe','\xbf',
  '\xc0','\xc1','\xc2','\xc3','\xc4','\xc5','\xc6','\xc7','\xc8','\xc9','\xca','\xcb','\xcc','\xcd','\xce','\xcf',
  '\xd0','\xd1','\xd2','\xd3','\xd4','\xd5','\xd6','\xd7','\xd8','\xd9','\xda','\xdb','\xdc','\xdd','\xde','\xdf',
  '\xc0','\xc1','\xc2','\xc3','\xc4','\xc5','\xc6','\xc7','\xc8','\xc9','\xca','\xcb','\xcc','\xcd','\xce','\xcf',
  '\xd0','\xd1','\xd2','\xd3','\xd4','\xd5','\xd6','\xd7','\xd8','\xd9','\xda','\xdb','\xdc','\xdd','\xde','\xdf',
];

public immutable char[256] koi8toupperTable = [
  '\x00','\x01','\x02','\x03','\x04','\x05','\x06','\x07','\x08','\x09','\x0a','\x0b','\x0c','\x0d','\x0e','\x0f',
  '\x10','\x11','\x12','\x13','\x14','\x15','\x16','\x17','\x18','\x19','\x1a','\x1b','\x1c','\x1d','\x1e','\x1f',
  '\x20','\x21','\x22','\x23','\x24','\x25','\x26','\x27','\x28','\x29','\x2a','\x2b','\x2c','\x2d','\x2e','\x2f',
  '\x30','\x31','\x32','\x33','\x34','\x35','\x36','\x37','\x38','\x39','\x3a','\x3b','\x3c','\x3d','\x3e','\x3f',
  '\x40','\x41','\x42','\x43','\x44','\x45','\x46','\x47','\x48','\x49','\x4a','\x4b','\x4c','\x4d','\x4e','\x4f',
  '\x50','\x51','\x52','\x53','\x54','\x55','\x56','\x57','\x58','\x59','\x5a','\x5b','\x5c','\x5d','\x5e','\x5f',
  '\x60','\x41','\x42','\x43','\x44','\x45','\x46','\x47','\x48','\x49','\x4a','\x4b','\x4c','\x4d','\x4e','\x4f',
  '\x50','\x51','\x52','\x53','\x54','\x55','\x56','\x57','\x58','\x59','\x5a','\x7b','\x7c','\x7d','\x7e','\x7f',
  '\x80','\x81','\x82','\x83','\x84','\x85','\x86','\x87','\x88','\x89','\x8a','\x8b','\x8c','\x8d','\x8e','\x8f',
  '\x90','\x91','\x92','\x93','\x94','\x95','\x96','\x97','\x98','\x99','\x9a','\x9b','\x9c','\x9d','\x9e','\x9f',
  '\xa0','\xa1','\xa2','\xb3','\xb4','\xa5','\xb6','\xb7','\xa8','\xa9','\xaa','\xab','\xac','\xbd','\xae','\xaf',
  '\xb0','\xb1','\xb2','\xb3','\xb4','\xb5','\xb6','\xb7','\xb8','\xb9','\xba','\xbb','\xbc','\xbd','\xbe','\xbf',
  '\xe0','\xe1','\xe2','\xe3','\xe4','\xe5','\xe6','\xe7','\xe8','\xe9','\xea','\xeb','\xec','\xed','\xee','\xef',
  '\xf0','\xf1','\xf2','\xf3','\xf4','\xf5','\xf6','\xf7','\xf8','\xf9','\xfa','\xfb','\xfc','\xfd','\xfe','\xff',
  '\xe0','\xe1','\xe2','\xe3','\xe4','\xe5','\xe6','\xe7','\xe8','\xe9','\xea','\xeb','\xec','\xed','\xee','\xef',
  '\xf0','\xf1','\xf2','\xf3','\xf4','\xf5','\xf6','\xf7','\xf8','\xf9','\xfa','\xfb','\xfc','\xfd','\xfe','\xff',
];

public immutable ubyte[32] koi8alphaTable = [
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xfe,0xff,0xff,0x07,0xfe,0xff,0xff,0x07,
  0x00,0x00,0x00,0x00,0xd8,0x20,0xd8,0x20,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,
];

public char dos2koi8 (char ch) pure nothrow @trusted @nogc {
  pragma(inline, true);
  return koi8from866Table.ptr[cast(int)ch];
}

public char win2koi8 (char ch) pure nothrow @trusted @nogc {
  pragma(inline, true);
  return koi8from1251Table.ptr[cast(int)ch];
}

public char koi8lower (char ch) pure nothrow @trusted @nogc {
  pragma(inline, true);
  return koi8tolowerTable.ptr[cast(int)ch];
}

public char koi8upper (char ch) pure nothrow @trusted @nogc {
  pragma(inline, true);
  return koi8toupperTable.ptr[cast(int)ch];
}

public bool koi8isAlpha (char ch) pure nothrow @trusted @nogc {
  pragma(inline, true);
  return ((koi8alphaTable.ptr[ch/8]&(1<<(ch%8))) != 0);
}


public bool koi8StrCaseEqu (const(char)[] s0, const(char)[] s1) pure nothrow @trusted @nogc {
  if (s0.length != s1.length) return false;
  foreach (immutable idx, char ch; s0) {
    if (koi8tolowerTable.ptr[ch] != koi8tolowerTable.ptr[s1.ptr[idx]]) return false;
  }
  return true;
}
