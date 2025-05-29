"""Fix file permissions."""

import argparse
import os
import stat

parser = argparse.ArgumentParser(
    prog="fix_permissions", description="Fixes file permissions"
)

parser.add_argument("filename")
parser.add_argument("-v", "--verbose", action="store_true")

args = parser.parse_args()

file_stat = os.lstat(args.filename)
file_stat = stat.S_IMODE(file_stat.st_mode)

# perm_dict = {
#     stat.S_IRUSR: "user read permission",
#     stat.S_IWUSR: "user write permission",
#     stat.S_IXUSR: "user execute permission",
#     stat.S_IRGRP: "group read permission",
#     stat.S_IWGRP: "group write permission",
#     stat.S_IXGRP: "group execute permission",
#     stat.S_IROTH: "other read permission",
#     stat.S_IWOTH: "other write permission",
#     stat.S_IXOTH: "other execute permission",
# }

# for k, v in perm_dict.items():
#     if file_stat & k:
#         print(v)
#     else:
#         print(f"no {v}")

if file_stat & stat.S_IXUSR:
    os.chmod(args.filename, file_stat | stat.S_IXGRP | stat.S_IXOTH)
    print(f"fixed execute permission on file {args.filename}")
