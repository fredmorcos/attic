#include <stdio.h>
#include <stdlib.h>

#define RED   0
#define GREEN 1
#define BLUE  2

#define VEHICLE(x) ((Vehicle *) x)

typedef struct _vehicle
{
  int wheels;
  int color;
} Vehicle;

typedef struct _bus
{
  Vehicle parent;
  int length;
} Bus;

void vehicle_print_info (Vehicle *v)
{
  printf("Vehicle: wheels = %d, color = %d\n",
	 v->wheels, v->color);
}

void bus_print_info (Bus *b)
{
  vehicle_print_info(VEHICLE(b));
  printf("Bus: length = %d\n", b->length);
}

Bus *bus_new (int wheels, int color, int length)
{
  Bus *tmp = (Bus *) malloc (sizeof (Bus));
  VEHICLE(tmp)->wheels = wheels;
  VEHICLE(tmp)->color  = color;
  tmp->length = length;
  return tmp;
}

int main (int argc, char **argv)
{
  Bus *bus = bus_new(4, RED, 20);
  bus_print_info(bus);
  free(bus);

  Bus bus2 = { .parent = { .wheels = 6, .color = GREEN }, .length = 10 };
  vehicle_print_info(VEHICLE(&bus2));
  bus_print_info(&bus2);

  return 0;
}
