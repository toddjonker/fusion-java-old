// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.Syntax.datumToSyntaxMaybe;


final class FusionEval
{
    private FusionEval() {}


    /**
     * The default evaluation handler, evaluating the given source
     * within the current namespace.
     *
     * @see <a href="http://docs.racket-lang.org/reference/eval.html#%28def._%28%28quote._~23~25kernel%29._current-eval%29%29">
         Racket's <code>eval</code></a>
     */
    static Object defaultEval(Evaluator eval, SyntaxValue source)
        throws FusionException
    {
        Namespace ns = eval.findCurrentNamespace();

        // TODO this should partial-expand and splice begins

        source = eval.expand(ns, source);
        CompiledForm compiled = eval.compile(ns, source);
        source = null; // Don't hold garbage

        return eval.eval(ns, compiled); // TODO TAIL
    }


    static Object callCurrentEval(Evaluator eval, SyntaxValue source)
        throws FusionException
    {
        return defaultEval(eval, source);
    }


    /**
     * Expands, compiles, and evaluates a single top-level form.
     * <p>
     * Equivalent to Racket's {@code eval} (but incomplete at the moment.)
     *
     * @param ns may be null to use {@link Evaluator#findCurrentNamespace()}.
     */
    static Object eval(Evaluator eval, SyntaxValue source, Namespace ns)
        throws FusionException
    {
        eval = eval.parameterizeCurrentNamespace(ns);

        // TODO FUSION-44 handle (module ...) properly
        source = eval.findCurrentNamespace().syntaxIntroduce(source);

        return callCurrentEval(eval, source); // TODO TAIL
    }


    /**
     * Like {@link #eval(Evaluator, SyntaxValue, Namespace)},
     * but does not enrich the source's lexical context.
     *
     * @param ns may be null to use {@link Evaluator#findCurrentNamespace()}.
     */
    static Object evalSyntax(Evaluator eval, SyntaxValue source, Namespace ns)
        throws FusionException
    {
        eval = eval.parameterizeCurrentNamespace(ns);

        return callCurrentEval(eval, source); // TODO TAIL
    }


    //========================================================================


    static final class EvalProc
        extends Procedure2
    {
        EvalProc()
        {
            //    "                                                                               |
            super("Evaluates a `top_form` within a `namespace`.  If `namespace` is null then the\n" +
                  "[`current_namespace`](namespace.html#current_namespace) parameter is used.\n" +
                  "\n" +
                  "The `top_form` must be a valid top-level syntactic form with respect to the\n" +
                  "bindings visible in the namespace.  The form is expanded, compiled, and\n" +
                  "evaluated, and its result is returned.  Any side effects made to the namespace\n" +
                  "will be visible to later evaluations.",
                  "top_form", "namespace");
        }

        @Override
        Object doApply(Evaluator eval, Object arg0, Object arg1)
            throws FusionException
        {
            SyntaxValue stx = datumToSyntaxMaybe(eval, null, arg0);
            if (stx == null)
            {
                throw argFailure("Syntax object or Ionizable data", 0, arg0, arg1);
            }

            Namespace ns;
            if (arg1 instanceof Namespace)
            {
                ns = (Namespace) arg1;
            }
            else if (isNullNull(eval, arg1))
            {
                ns = null;
            }
            else
            {
                throw argFailure("namespace or null", 1, arg0, arg1);
            }

            return FusionEval.eval(eval, stx, ns);  // TODO TAIL
        }
    }
}
