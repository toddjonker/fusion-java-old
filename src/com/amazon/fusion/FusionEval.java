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
     * @param topLevelForm is not enriched with lexical information.
     *
     * @see <a href="http://docs.racket-lang.org/reference/eval.html#%28def._%28%28quote._~23~25kernel%29._current-eval%29%29">
         Racket's <code>eval</code></a>
     */
    static Object defaultEval(Evaluator eval, SyntaxValue topLevelForm)
        throws FusionException
    {
        Namespace ns = eval.findCurrentNamespace();

        {
            // TODO FUSION-33 this should partial-expand and splice begins
            Expander expander = new Expander(eval);
            topLevelForm = expander.expand(ns, topLevelForm);
        }

        CompiledForm compiled = eval.compile(ns, topLevelForm);
        topLevelForm = null; // Don't hold garbage

        return eval.eval(ns, compiled); // TODO TAIL
    }


    static Object callCurrentEval(Evaluator eval, SyntaxValue topLevelForm)
        throws FusionException
    {
        return defaultEval(eval, topLevelForm);
    }


    /**
     * Expands, compiles, and evaluates a single top-level form.
     * <p>
     * Equivalent to Racket's {@code eval} (but incomplete at the moment.)
     *
     * @param ns may be null to use {@link Evaluator#findCurrentNamespace()}.
     */
    static Object eval(Evaluator eval, SyntaxValue topLevelForm, Namespace ns)
        throws FusionException
    {
        eval = eval.parameterizeCurrentNamespace(ns);

        // TODO FUSION-44 handle (module ...) properly
        topLevelForm = eval.findCurrentNamespace().syntaxIntroduce(topLevelForm);

        return callCurrentEval(eval, topLevelForm); // TODO TAIL
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
        extends Procedure
    {
        EvalProc()
        {
            //    "                                                                               |
            super("Evaluates a `top_form` within a `namespace`.  If `namespace` is absent then the\n" +
                  "[`current_namespace`](namespace.html#current_namespace) parameter is used.\n" +
                  "\n" +
                  "The `top_form` must be a valid top-level syntactic form with respect to the\n" +
                  "bindings visible in the namespace.  The form is expanded, compiled, and\n" +
                  "evaluated, and its result is returned.  Any side effects made to the namespace\n" +
                  "will be visible to later evaluations.",
                  "top_form", "[namespace]");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityRange(1, 2, args);

            SyntaxValue stx = datumToSyntaxMaybe(eval, null, args[0]);
            if (stx == null)
            {
                throw argFailure("Syntax object or Ionizable data", 0, args);
            }

            Namespace ns = null;
            if (args.length == 2)
            {
                if (args[1] instanceof Namespace)
                {
                    ns = (Namespace) args[1];
                }
                else
                {
                    throw argFailure("namespace", 1, args);
                }
            }

            return FusionEval.eval(eval, stx, ns);  // TODO TAIL
        }
    }
}
