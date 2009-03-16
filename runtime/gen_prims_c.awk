BEGIN {
	n = 0;
	print "#include \"mlvalues.h\"";
	print "#include \"prims.h\"";
};

// {
	defs[n] = $0; n++;
};

END {
	for (x = 1; x < n; x++)
		printf "extern value %s();\n", defs[x];
	printf("c_primitive cprim[] = {\n");
	for (x = 1; x < n; x++) {
		printf "  %s,\n", defs[x];
	}

	printf("  0};\n");
	printf("char *names_of_cprim[] = {\n");
	for (x = 1; x < n; x++)
		printf "  \"%s\",\n", defs[x];
        printf("  0};\n");
}
