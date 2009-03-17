BEGIN {
	n = 0;
	print "(* AUTOGENERATED by gen_predef.sml.awk *)"
};

/^\#.*$/ { }

/^[A-Za-z].*$/ {
	defs[n] = $0; n++;
};

END {
	printf "val primitives_table = #[\n";
	for (x = 0; x < n-1; x++)
		printf "  \"%s\",\n", defs[x];
	printf "  \"%s\"];\n", defs[n-1]
}
