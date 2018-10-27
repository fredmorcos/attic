/* This file is part of powerconf.
 *
 * powerconf is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * powerconf is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with powerconf.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>
#include <stdlib.h>
#include "config.h"
#include "debug.h"
#include "tunable.h"

int main (int argc, char **argv)
{
  int i;
  int value;
  char *endptr[1];

  tunable_t tunables[] = {
    {"sched_mc",
     "/sys/devices/system/cpu/sched_mc_power_savings",
     0, 2, 1, 0, {NULL}},
    {"sched_smt",
     "/sys/devices/system/cpu/sched_smt_power_savings",
     0, 2, 1, 0, {NULL}},
    {"laptop_mode",
     "/proc/sys/vm/laptop_mode",
     0, 5, 1, 0, {NULL}},
    {"swappiness",
     "/proc/sys/vm/swappiness",
     1, 99, 0, 0, {NULL}},
    {"vfs_cache_pressure",
     "/proc/sys/vm/vfs_cache_pressure",
     50, 150, 0, 0, {NULL}},
    {"nmi_watchdog",
     "/proc/sys/kernel/nmi_watchdog",
     0, 1, 0, 0, {NULL}},
    {"cpu_freq",
     "/sys/devices/system/cpu/cpu*/cpufreq/scaling_governor",
     0, 2, 0, 1, {"ondemand", "ondemand", "performance", NULL}},
    {"pcie_aspm",
     "/sys/module/pcie_aspm/parameters/policy",
     0, 1, 0, 1, {"powersave", "performance", NULL}}
  };

  if (argc < 2)
    {
      fprintf (stderr, "Missing argument\n");
      goto print_usage;
    }

  value = strtol(argv[1], endptr, 10);

  if (*endptr == argv[1])
    {
      fprintf (stderr, "Invalid argument: %s\n", argv[1]);
      goto print_usage;
    }

  if (value < MIN_GLOBAL_LEVEL)
    {
      fprintf (stderr, "Invalid argument: %s\n", argv[1]);
      fprintf (stderr, "  Bad parameter: value < MIN_GLOBAL_LEVEL (%d)\n",
	       MIN_GLOBAL_LEVEL);
      goto print_usage;
    }

  if (value > MAX_GLOBAL_LEVEL)
    {
      fprintf (stderr, "Invalid argument: %s\n", argv[1]);
      fprintf (stderr, "  Bad parameter: value > MAX_GLOBAL_LEVEL (%d)\n",
	       MAX_GLOBAL_LEVEL);
      goto print_usage;
    }

  fprintf (stdout, LOGSTR ("Using power-level %d"),
	   __FILE__, __FUNCTION__, __LINE__, value);

  for (i = 0; i < sizeof (tunables) / sizeof (tunable_t); i++)
    {
      if (write_tunable (&tunables[i], value) == EXIT_FAILURE)
	fprintf (stdout, LOGSTR ("%s failed"),
		 __FILE__, __FUNCTION__, __LINE__,
		 tunables[i].name);
    }

  return EXIT_SUCCESS;

 print_usage:
  fprintf (stderr, "usage:\n");
  fprintf (stderr, "  powerconf <power-level>\n\n");
  fprintf (stderr, "  power-level is a value between %d and %d\n",
	   MIN_GLOBAL_LEVEL, MAX_GLOBAL_LEVEL);
  fprintf (stderr, "  %5d -> powersaving\n", MIN_GLOBAL_LEVEL);
  fprintf (stderr, "  %5d -> ondemand/dynamic\n",
	   (MAX_GLOBAL_LEVEL - MIN_GLOBAL_LEVEL) / 2);
  fprintf (stderr, "  %5d -> performance\n", MAX_GLOBAL_LEVEL);

  return EXIT_FAILURE;
}
