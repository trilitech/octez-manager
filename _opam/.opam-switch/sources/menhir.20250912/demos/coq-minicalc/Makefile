MENHIR            := menhir
MENHIRFLAGS       := --coq

Makefile.coq:
	@ coq_makefile -f _CoqProject -o Makefile.coq

Parser.v : Parser.vy
	@ $(MENHIR) $(MENHIRFLAGS) $<

ifneq ($(MAKECMDGOALS),clean)
-include Makefile.coq
endif

.PHONY: clean
clean::
	@ git clean -fdX
