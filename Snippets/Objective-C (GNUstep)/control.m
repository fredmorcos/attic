/* W.J. Antel Jr.  12/12/96
 * ** Compile with gnu's gcc:  gcc hello.m -lobjc
 * */
#include 

@interface Control:Object

{
}
- world;
void main();

@end

@implementation Control

- world
{
	printf("Hello, World!\n");
}

void main()
{
	id hello;

	hello=[Control new];

	for(;;)
		[hello world];
}
@end
