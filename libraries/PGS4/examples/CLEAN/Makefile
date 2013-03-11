# -----------------------------------------------------------------
MAKE=make
COMPILE=gfortran
# -----------------------------------------------------------------
# Makefile body
# -----------------------------------------------------------------

../clean_output: clean_lhco_output.f
	@echo
	@echo ----- Now compiling output cleaner -----
	@echo
	$(COMPILE) -g -fno-automatic clean_lhco_output.f \
	-o ../clean_output; chmod +x ../clean_output
