// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code begin} syntactic form.
 */
final class BeginForm
    extends SyntacticForm
{
    static SyntaxSexp makeSyntax(Evaluator eval, SyntaxSequence seq, int from)
        throws FusionException
    {
        SyntaxValue begin = eval.getGlobalState().myKernelBeginIdentifier;
        int size = seq.size();
        if (size <= from)
        {
            return SyntaxSexp.make(eval, begin);
        }

        SyntaxValue[] subforms = new SyntaxValue[size - from + 1];
        subforms[0] = begin;

        for (int i = from; i < size; i++)
        {
            SyntaxValue bodyForm = seq.get(eval, i);
            subforms[i - from + 1] = bodyForm;
        }

        return SyntaxSexp.make(eval, subforms);
    }


    BeginForm()
    {
        //    "                                                                               |
        super("expr ...",
              "Evaluates the `expr`s in order, returning the final result.  The last `expr` is\n"
            + "in tail position.  If there are no `expr`s the result is void.");
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        // At module context, we should've spliced this into the module body.
        assert ! (expander.isModuleContext());
        // TODO FUSION-33 handle splicing in top-level context
        // TODO FUSION-36 handle splicing in internal-defn context

        int size = stx.size();

        SyntaxValue[] expandedChildren = new SyntaxValue[size];
        expandedChildren[0] = stx.get(eval, 0);

        for (int i = 1; i < size; i++)
        {
            SyntaxValue subform = stx.get(eval, i);
            expandedChildren[i] = expander.expandExpression(env, subform);
        }
        return stx.copyReplacingChildren(eval, expandedChildren);
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        return compile(eval, env, stx, 1, stx.size());
    }


    static CompiledForm compile(Evaluator eval, Environment env,
                                SyntaxSexp stx, int from, int to)
        throws FusionException
    {
        int size = to - from;

        if (size == 0) return CompiledVoid.SINGLETON;

        if (size == 1) return eval.compile(env, stx.get(eval, from));

        CompiledForm[] subforms = eval.compile(env, stx, from, to);
        return new CompiledBegin(subforms);
    }


    static CompiledForm compile(Evaluator eval, Environment env,
                                SyntaxSexp stx, int from)
        throws FusionException
    {
        return compile(eval, env, stx, from, stx.size());
    }


    //========================================================================


    private static final class CompiledBegin
        implements CompiledForm
    {
        final CompiledForm[] myBody;

        CompiledBegin(CompiledForm[] body)
        {
            myBody = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            final int last = myBody.length - 1;
            for (int i = 0; i < last; i++)
            {
                CompiledForm form = myBody[i];
                eval.eval(store, form);
            }

            CompiledForm form = myBody[last];
            return eval.bounceTailForm(store, form);
        }
    }
}
