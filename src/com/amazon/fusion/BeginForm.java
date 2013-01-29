// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code begin} syntactic form.
 */
final class BeginForm
    extends SyntacticForm
{
    static SyntaxSexp makeSyntax(Evaluator eval, SyntaxSequence seq, int from)
    {
        SyntaxValue begin = eval.makeKernelIdentifier("begin");
        int size = seq.size();
        if (size <= from)
        {
            return SyntaxSexp.make(begin);
        }

        SyntaxValue[] subforms = new SyntaxValue[size - from + 1];
        subforms[0] = begin;

        for (int i = from; i < size; i++)
        {
            SyntaxValue bodyForm = seq.get(i);
            subforms[i - from + 1] = bodyForm;
        }

        SyntaxSexp beginForm = SyntaxSexp.make(/* location */ null, subforms);
        return beginForm;
    }


    BeginForm()
    {
        //    "                                                                               |
        super("EXPR ...",
              "Evaluates the EXPRs in order, returning the final result.\n" +
              "The last EXPR is in tail position. If there are no EXPRs the result is void.");
    }


    @Override
    SyntaxValue expand(Evaluator eval, Expander ctx, Environment env, SyntaxSexp source)
        throws FusionException
    {
        int size = source.size();

        SyntaxValue[] expandedChildren = new SyntaxValue[size];
        expandedChildren[0] = source.get(0);

        for (int i = 1; i < size; i++)
        {
            SyntaxValue subform = source.get(i);
            expandedChildren[i] = ctx.expand(env, subform);
        }
        return SyntaxSexp.make(source.getLocation(), expandedChildren);
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        return compile(eval, env, source, 1, source.size());
    }


    static CompiledForm compile(Evaluator eval, Environment env,
                                SyntaxSexp source, int from, int to)
        throws FusionException
    {
        int size = to - from;

        if (size == 0) return CompiledVoid.SINGLETON;

        if (size == 1) return eval.compile(env, source.get(from));

        CompiledForm[] subforms = eval.compile(env, source, from, to);
        return new CompiledBegin(subforms);
    }


    static CompiledForm compile(Evaluator eval, Environment env,
                                SyntaxSexp source, int from)
        throws FusionException
    {
        return compile(eval, env, source, from, source.size());
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
