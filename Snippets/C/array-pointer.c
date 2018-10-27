int main (const int argc, char *argv[argc - 1]) {
  const int arr1[] = {1, 2, 3, 4, 5};
  const int arr2[] = {1, 2, 3, 4, 5, 6};

  const int (*a)[5] = &arr1;
  const int (*b)[ ] = &arr2;
  const int (*c)[5] = &arr2;                 /* should issue a warning */

  b = &arr1;
  a = &arr2;                                 /* should issue a warning */

  return 0;
}
