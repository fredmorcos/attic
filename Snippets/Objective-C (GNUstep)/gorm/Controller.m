/* All Rights reserved */

#include <AppKit/AppKit.h>
#include "Controller.h"

@implementation Controller


- (void) getCurrentTime: (id)sender
{
  NSCalendarDate *date = [NSCalendarDate date];
  [date setCalendarFormat: @"%H:%M:%S"];
  [timeLabel setStringValue: [date description]];
}

@end
