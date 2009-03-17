BEGIN {
	n = 0;
	print "(* AUTOGENERATED by gen_opcodes.sml.awk *)"
};

/^\#.*$/ { }

/^[A-Za-z].*$/ {
	defs[n] = $0; n++;
};

END {
	for (x = 0; x < n; x++)
		printf "val %s = %d;\n", defs[x], x;
}