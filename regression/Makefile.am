## Makefile.am -- Process this file with automake to produce Makefile.in

EXTRA_DIST = defs $(TESTS)
MAINTAINERCLEANFILES = Makefile.in
testsubdir = testSubDir
CLEANFILES = tests/*.ui tests/*.uo
TESTS_ENVIRONMENT = ${srcdir}/run-test-ok
TESTS = tests/1.sml \
	tests/2.sml \
	tests/3.sml \
	tests/4.sml \
	tests/6.sml \
	tests/7.sml \
	tests/8.sml \
	tests/9.sml \
	tests/10.sml \
	tests/11.sml \
	tests/12.sml \
	tests/13.sml \
	tests/14.sml \
	tests/15.sml \
	tests/21.sml \
	tests/22.sml \
	tests/array2.sml \
	tests/array3.sml \
	tests/array4.sml \
	tests/array5.sml \
	tests/array7.sml \
	tests/big-array.sml \
	tests/binio.sml \
	tests/bool-triple.sml \
	tests/cases.sml \
	tests/cobol.sml \
	tests/char0.sml \
	tests/check_arrays.sml \
	tests/circular.sml \
	tests/comment-end.sml \
	tests/constraint.sml \
	tests/datatype-with-free-tyvars.sml \
	tests/date.sml \
	tests/default-overloads.sml \
	tests/deep-flatten.sml \
	tests/down.sml \
	tests/exhaustive.sml \
	tests/exn.sml \
	tests/exn2.sml \
	tests/expansive-valbind.sml \
	tests/exponential.sml \
	tests/fact.sml \
	tests/fast2.sml \
	tests/fast.sml \
	tests/ffi.sml \
	tests/fft.sml \
	tests/flat-array.2.sml \
	tests/flat-array.sml \
	tests/flat-vector.sml \
	tests/format.sml \
	tests/FuhMishra.sml \
	tests/functor.sml \
	tests/hello-world.sml \
	tests/int.sml \
	tests/jump.sml \
	tests/kitdangle3.sml \
	tests/kitdangle.sml \
	tests/kitfib35.sml \
	tests/kitkbjul9.sml \
	tests/kitlife35u.sml \
	tests/kitloop2.sml \
	tests/kitmandelbrot.sml \
	tests/kitqsort.sml \
	tests/kitreynolds2.sml \
	tests/kitreynolds3.sml \
	tests/kitsimple.sml \
	tests/kittmergesort.sml \
	tests/kkb36c.sml \
	tests/kkb_eq.sml \
	tests/klife_eq.sml \
	tests/known-case0.sml \
	tests/known-case1.sml \
	tests/lambda-list-ref.sml \
	tests/layout.sml \
	tests/lex.sml \
	tests/lib.sml \
	tests/life.sml \
	tests/listpair.sml \
	tests/list.sml \
	tests/llv.sml \
	tests/local-ref.sml \
	tests/msort.sml \
	tests/nested-loop.sml \
	tests/nonexhaustive.sml \
	tests/os-exit.sml \
	tests/open.sml \
	tests/pat.sml \
	tests/poly-equal.sml \
	tests/polymorphic-recursion.sml \
	tests/print-self.sml \
	tests/ref-flatten.3.sml \
	tests/ref-flatten.5.sml \
	tests/ref-flatten.6.sml \
	tests/ref-flatten.sml \
	tests/rem-unused.1.sml \
	tests/ring.sml \
	tests/semicolon.sml \
	tests/string.sml \
	tests/string2.sml \
	tests/stringcvt.sml \
	tests/substring.sml \
	tests/tak.sml \
	tests/taut.sml \
	tests/testdyn1.sml \
	tests/textio.sml \
	tests/textio.2.sml \
	tests/time3.sml \
	tests/type-check.sml \
	tests/tststrcmp.sml \
	tests/unary.2.sml \
	tests/unary.sml \
	tests/useless-string.sml \
	tests/valrec.sml \
	tests/vector2.sml \
	tests/vector3.sml \
	tests/vector4.sml \
	tests/vector-loop.sml \
	tests/where.sml \
	tests/withtype.sml \
	tests/word.sml

#	tests/time2.sml \ <- needs intinf first.



## All these fails on mosml. Some have explanations added
#	tests/5.sml \
#	tests/20.sml \
#	tests/abstype.sml \
#	tests/suspend.sml \
#	tests/generate/all-overloads.sml \
#	tests/time.sml \
#	tests/string.fromString.sml \
#	tests/general.sml \
#	tests/weak.sml \
#	tests/sigs.sml \
#	tests/rlimit.sml \
#	tests/word-all.sml \
#	tests/conv.sml \
#	tests/12.sml \
#	tests/array6.sml \
#	tests/where-and.sml \
#	tests/word8vector.sml \
#	tests/type-check.sml \
#	tests/slow2.sml \
#	tests/same-fringe.sml \
#	tests/mutex.sml
#	tests/spawn.sml
#	tests/mlton.word.sml
#	tests/ffi-opaque.sml
#	tests/math.sml \ -- fails on mosml
#	tests/23.sml \ -- nonterminating on purpose
#	tests/ex.sml \ -- nonterminating on purpose
#	tests/only-one-exception.sml \ -- nonterminating on purpose
#	tests/dead.sml \ -- nonterminating
#	tests/cycle.sml \
#	tests/17.sml \
#	tests/19.sml \
#	tests/int-inf.log2.sml \
#	tests/7.sml \
#	tests/fixed-integer.sml \
#	tests/once.sml \
#	tests/gc-collect.sml \
#	tests/posix-procenv.sml
#	tests/warn/undetermined.sml
#	tests/warn/warning.sml
# tests/basis-sharing.sml \
# tests/modules.sml \
# tests/array.sml \
# tests/unixpath.sml \
# tests/callcc.sml \
# tests/signals2.sml \
# tests/int-inf.2.sml \
# tests/sharing.sml \
# tests/flexrecord.2.sml
# tests/mlton.share.sml \
# tests/parse.sml \
# tests/real-int.sml \
# tests/testMatrix.sml \
# tests/thread-switch-share.sml \
# tests/bytechar.sml \
# tests/callcc2.sml
# tests/smith-normal-form.sml -- don't enable this one, it is not going to terminate this lifetime
# tests/filesys.sml \
# tests/size2.sml \
# tests/scon.sml \
# tests/prodcons.sml \
# tests/int-inf.compare.sml \
# tests/int-inf.3.sml \
# tests/pack-real.2.sml \
# tests/11.sml \
# tests/eq.sml \
# tests/overloading.sml \
# tests/exn2.sml \
# tests/int-inf.4.sml \
# tests/thread0.sml \
# tests/abcde.sml \
# tests/asterisk.sml \
# tests/flexrecord.sml \
# tests/undetermined.sml \
# tests/slower.sml \
# tests/ref-flatten.2.sml \
# tests/timeout.sml \
# tests/int-inf.5.sml \
# tests/size.sml \
# tests/10.sml \
# tests/pack.sml
#	tests/thread-switch.sml \
#	tests/comment-end.sml
# tests/abstype.sml \
# tests/18.sml
# tests/9.sml \
# tests/posix-exit.sml \
# tests/slow.sml \
# tests/opaque.sml \
# tests/real.sml \
# tests/ref-flatten.4.sml \
# tests/int-overflow.sml \
# tests/command-line.sml \
# tests/real-algsimp.sml \
# tests/char.scan.sml \
# tests/all-overloads.sml \
# tests/ieee-real.sml \
# tests/functor.sml \
# tests/opaque2.sml
# tests/int-inf.1.sml \
# tests/callcc3.sml \
# tests/string.sml \
# tests/char0.sml \
# tests/lib.sml \
# tests/nextAfter.sml \
# tests/harmonic.sml \
# tests/typespec.sml \
# tests/exnHistory3.sml \
# tests/pack-real.sml \
# tests/id.sml \
# tests/wordn-array.sml \
# tests/signals.sml \
# tests/int-inf.0.sml \
# tests/expansive-valbind.sml \
# tests/thread2.sml \
# tests/cmdline.sml \
# tests/mlton.overload.sml \
# tests/time4.sml \
# tests/substring-overflow.sml \
# tests/conv2.sml \
# tests/ftruncate.sml
# tests/thread1.sml \
# tests/default-overloads.sml \
# tests/eqtype.sml \
# tests/ex.sml \
# tests/widechar.sml \
# tests/flat-array.3.sml \
# tests/serialize.sml \
# tests/14.sml
#tests/thread-switch-size.sml \
#tests/exnHistory.sml \
#tests/vector.sml \
#tests/word8array.sml \
#tests/socket.sml \
#tests/pseudokit.sml
# tests/nonterminate/8.sml \
# tests/int-inf.bitops.sml \
# tests/substring.sml \
# tests/real-str.sml \
# tests/grow-raise.sml \
# tests/pack-word.sml \
# tests/poly-equal.2.sml \
# tests/16.sml \
# tests/taut.sml \
# tests/msort.sml




### These are the tests that has not been added to the suite yet
## BENCHMARKS
#  Some of these are wickedly expensive to run. But we ought to have them in a test run.

# benchmarks/ratio-regions.sml
# benchmarks/logic.sml
# benchmarks/tailfib.sml
# benchmarks/tsp.sml
# benchmarks/nucleic.sml
# benchmarks/fib.sml
# benchmarks/peek.sml
# benchmarks/imp-for.sml
# benchmarks/zern.sml
# benchmarks/fft.sml
# benchmarks/barnes-hut.sml
# benchmarks/lexgen.sml
# benchmarks/fxp.sml
# benchmarks/wc-input1.sml
# benchmarks/tensor.sml
# benchmarks/hamlet.sml
# benchmarks/smith-normal-form.sml
# benchmarks/mpuz.sml
# benchmarks/DLXSimulator.sml
# benchmarks/vector-concat.sml
# benchmarks/life.sml
# benchmarks/tak.sml
# benchmarks/matrix-multiply.sml
# benchmarks/output1.sml
# benchmarks/ray.sml
# benchmarks/vector-rev.sml
# benchmarks/count-graphs.sml
# benchmarks/mlyacc.sml
# benchmarks/merge.sml
# benchmarks/zebrapig.sml
# benchmarks/vliw.sml
# benchmarks/boyer.sml
# benchmarks/tyan.sml
# benchmarks/wc-scanStream.sml
# benchmarks/mandelbrot.sml
# benchmarks/checksum.sml
# benchmarks/psdes-random.sml
# benchmarks/md5.sml
# benchmarks/knuth-bendix.sml
# benchmarks/raytrace.sml
# benchmarks/DATA/hamlet-input.sml
# benchmarks/model-elimination.sml
# benchmarks/flat-array.sml
# benchmarks/simple.sml

# tests/library/libm1.sml
# tests/library/libm5.sml
# tests/library/libm4.sml
# tests/library/libm2.sml
# tests/library/libm3.sml
# tests/library/check.sml
# tests/int-overflow.sml
# tests/word.sml \   -- Currently fails on mosml

## Failing tests -- should be added in due time
# tests/fail/datatype.1.sml
# tests/fail/datatype.4.sml
# tests/fail/datatype-where-complex.sml
# tests/fail/modules.43.sml
# tests/fail/modules.37.sml
# tests/fail/modules.32.sml
# tests/fail/overloading-context.2.sml
# tests/fail/modules.22.sml
# tests/fail/structure-and.sml
# tests/fail/pat.sml
# tests/fail/type.1.sml
# tests/fail/modules.6.sml
# tests/fail/modules.13.sml
# tests/fail/equality-types.sml
# tests/fail/datatype.2.sml
# tests/fail/type-use-before-def.sml
# tests/fail/exp.2.sml
# tests/fail/modules.30.sml
# tests/fail/duplicate-tyvar.sml
# tests/fail/it.2.sml
# tests/fail/type.sml
# tests/fail/modules.24.sml
# tests/fail/modules.17.sml
# tests/fail/sharing.sml
# tests/fail/modules.3.sml
# tests/fail/modules.4.sml
# tests/fail/modules.34.sml
# tests/fail/modules.38.sml
# tests/fail/functor.1.sml
# tests/fail/modules.21.sml
# tests/fail/overloading-context.3.sml
# tests/fail/modules.16.sml
# tests/fail/2.sml
# tests/fail/tyvar-scope.1.sml
# tests/fail/1.sml
# tests/fail/modules.2.sml
# tests/fail/functor-and.sml
# tests/fail/modules.25.sml
# tests/fail/modules.50.sml
# tests/fail/modules.20.sml
# tests/fail/escaping-datatype.sml
# tests/fail/modules.12.sml
# tests/fail/it.1.sml
# tests/fail/functor-generative-equality.sml
# tests/fail/type.2.sml
# tests/fail/modules.19.sml
# tests/fail/modules.36.sml
# tests/fail/equal.sml
# tests/fail/val-rec-constructor.sml
# tests/fail/pat.3.sml
# tests/fail/modules.5.sml
# tests/fail/val-rec-constructor.2.sml
# tests/fail/exp.sml
# tests/fail/exp.8.sml
# tests/fail/modules.39.sml
# tests/fail/modules.8.sml
# tests/fail/modules.33.sml
# tests/fail/overloading-context.6.sml
# tests/fail/modules.46.sml
# tests/fail/constant-too-big.sml
# tests/fail/modules.9.sml
# tests/fail/modules.26.sml
# tests/fail/dec.sml
# tests/fail/free-type-variable.sml
# tests/fail/modules.40.sml
# tests/fail/overloading-context.4.sml
# tests/fail/modules.51.sml
# tests/fail/modules.10.sml
# tests/fail/modules.49.sml
# tests/fail/exp.4.sml
# tests/fail/exp.7.sml
# tests/fail/modules.35.sml
# tests/fail/modules.42.sml
# tests/fail/sharing.2.sml
# tests/fail/4.sml
# tests/fail/exp.3.sml
# tests/fail/modules.27.sml
# tests/fail/modules.45.sml
# tests/fail/sig.1.sml
# tests/fail/5.sml
# tests/fail/rank.sml
# tests/fail/modules.15.sml
# tests/fail/tyvar-scope.3.sml
# tests/fail/sig.2.sml
# tests/fail/pat.1.sml
# tests/fail/modules.11.sml
# tests/fail/modules.31.sml
# tests/fail/modules.44.sml
# tests/fail/modules.48.sml
# tests/fail/overloading-context.1.sml
# tests/fail/tyvar-scope.2.sml
# tests/fail/modules.14.sml
# tests/fail/modules.28.sml
# tests/fail/modules.41.sml
# tests/fail/exp.5.sml
# tests/fail/modules.29.sml
# tests/fail/signature-and.sml
# tests/fail/exp.6.sml
# tests/fail/exception.1.sml
# tests/fail/modules.1.sml
# tests/fail/modules.47.sml
# tests/fail/modules.18.sml
# tests/fail/3.sml
# tests/fail/eqtype.1.sml
# tests/fail/pat.2.sml
# tests/fail/datatype.3.sml
# tests/fail/overloading-context.5.sml
# tests/fail/modules.7.sml
# tests/fail/modules.23.sml
# tests/fail/exp.1.sml

distclean-local:
	-rm -fr $(testsubdir)

