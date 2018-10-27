#include "utils.h"

Utils::Utils()
{
}

QString Utils::sizeIntToString(double size)
{
	QString format = "bytes";

	if (size >= 1024)
	{
		size /= 1024;
		format = "KB";
	}

	if (size >= 1024)
	{
		size /= 1024;
		format = "MB";
	}

	if (size >= 1024)
	{
		size /= 1024;
		format = "GB";
	}

	if (size >= 1024)
	{
		size /= 1024;
		format = "TB";
	}

	return QString("%1").number(size, 'f', 1) + " " + format;
}
