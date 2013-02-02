// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.Syntax.datumToSyntaxMaybe;
import static com.amazon.fusion.Syntax.isSyntax;


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
    @SuppressWarnings("javadoc")
    private static Object defaultEval(Evaluator eval, Object topLevelForm)
        throws FusionException
    {
        Namespace ns = eval.findCurrentNamespace();

        SyntaxValue stx;
        if (Syntax.isSyntax(eval, topLevelForm))
        {
            stx = (SyntaxValue) topLevelForm;
        }
        else
        {
            stx = datumToSyntaxMaybe(eval, null, topLevelForm);
            if (stx == null)
            {
                throw new ArgTypeFailure("default_eval_handler",
                                         "Syntax object or Ionizable data",
                                         0, topLevelForm);
            }
            stx = enrich(eval, stx);
        }

        {
            // TODO FUSION-33 this should partial-expand and splice begins
            Expander expander = new Expander(eval);
            stx = expander.expand(ns, stx);
        }

        CompiledForm compiled = eval.compile(ns, stx);
        stx = null; // Don't hold garbage

        return eval.eval(ns, compiled); // TODO TAIL
    }


    /**
     * Placeholder so we can later add current-eval parameter.
     */
    static Object callCurrentEval(Evaluator eval, Object topLevelForm)
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
     * @param topLevelForm will be enriched.
     */
    static Object eval(Evaluator eval, Object topLevelForm, Namespace ns)
        throws FusionException
    {
        eval = eval.parameterizeCurrentNamespace(ns);

        if (isSyntax(eval, topLevelForm))
        {
            topLevelForm = enrich(eval, (SyntaxValue) topLevelForm);
        }

        return callCurrentEval(eval, topLevelForm); // TODO TAIL
    }


    /**
     * Like {@link #eval(Evaluator, Object, Namespace)},
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


    /**
     * Enriches a syntax object "in the same way as eval", using the current
     * namespace.
     */
    private static SyntaxValue enrich(Evaluator eval, SyntaxValue topLevelForm)
    {
        Namespace ns = eval.findCurrentNamespace();

        // TODO FUSION-44 handle (module ...) properly
        topLevelForm = ns.syntaxIntroduce(topLevelForm);
        return topLevelForm;
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

            return FusionEval.eval(eval, args[0], ns);  // TODO TAIL
        }
    }
}
