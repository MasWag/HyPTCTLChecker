EF_SIZE := 1 2 3 4 5
AU_SIZE := 1 2 3 4 5
HYPER_PROPS = $(EF_SIZE:%=count-sugar-EF-%.hyper-imiprop) $(AU_SIZE:%=count-sugar-AU-%.hyper-imiprop)
IMIS = $(EF_SIZE:%=/tmp/coffee-EF-%.imi) $(AU_SIZE:%=/tmp/coffee-AU-%.imi)
all: $(HYPER_PROPS)
imi: $(IMIS)

count-sugar-EF-%.hyper-imiprop: generate-count-sugar-EF.hyper-imiprop.sh
# Extract the number of counts from the filename
	$(eval NUM := $(subst .hyper-imiprop, , $(subst count-sugar-EF-, , $@)))
	bash $< $(NUM) > $@

count-sugar-AU-%.hyper-imiprop: generate-count-sugar-AU.hyper-imiprop.sh
# Extract the number of counts from the filename
	$(eval NUM := $(subst .hyper-imiprop, , $(subst count-sugar-AU-, , $@)))
	bash $< $(NUM) > $@

/tmp/coffee-EF-%.imi: coffee.hyper-imi count-sugar-EF-%.hyper-imiprop
	dune exec HyPTCTLChecker $^ $(basename $@)

/tmp/coffee-AU-%.imi: coffee.hyper-imi count-sugar-AU-%.hyper-imiprop
	dune exec HyPTCTLChecker $^ $(basename $@)

clean:
	$(RM) $(HYPER_PROPS) $(IMIS)

.PHONY: all clean imi
