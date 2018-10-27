template <typename t>

t max (t v1, t v2, t v3)
{
	t max_val = v1;

	if (v2 > max_val)
		max_val = v2;

	if (v3 > max_val)
		max_val = v3;
	
	return max_val;
}
