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
// VFS config defs (somewhat similar to autocrap's config.h)
module iv.vfs.config /*is aliced*/;

public:
version(Windows) {
  enum VFS_SHITDOZE = true;
  enum VFS_NORMAL_OS = false;
} else version(Posix) {
  enum VFS_SHITDOZE = false;
  enum VFS_NORMAL_OS = true;
} else {
  static assert(false, "iv.vfs: not shitdoze and not posix? O_O");
}
