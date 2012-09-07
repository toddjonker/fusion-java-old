// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code begin} syntactic form.
 */
final class BeginKeyword
    extends KeywordValue
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


    BeginKeyword()
    {
        //    "                                                                               |
        super("EXPR ...",
              "Evaluates the EXPRs in order, returning the final result.\n" +
              "The last EXPR is in tail position. If there are no EXPRs the result is undef.");
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        int size = source.size();

        SyntaxValue[] expandedChildren = new SyntaxValue[size];
        expandedChildren[0] = source.get(0);

        for (int i = 1; i < size; i++)
        {
            SyntaxValue subform = source.get(i);
            expandedChildren[i] = subform.prepare(eval, env);
        }
        return SyntaxSexp.make(source.getLocation(), expandedChildren);
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        final int size = source.size();

        CompiledForm compiled;
        if (size == 1)
        {
            // TODO FUSION-33 this shouldn't happen after begin-lifting
            compiled = new CompiledUndef();  // TODO singleton
        }
        else
        {
            compiled = compile(eval, env, source, 1, size);
        }
        return compiled;
    }


    static CompiledForm compile(Evaluator eval, Environment env,
                                SyntaxSexp source, int from, int to)
        throws FusionException
    {
        int size = to - from;

        CompiledForm compiled;
        if (size == 1)
        {
            compiled = eval.compile(env, source.get(from));
        }
        else
        {
            CompiledForm[] subforms = new CompiledForm[size];
            for (int i = from; i < to; i++)
            {
                SyntaxValue subform = source.get(i);
                subforms[i - from] = eval.compile(env, subform);
            }
            compiled = new CompiledBegin(subforms);
        }

        return compiled;
    }


    static CompiledForm compile(Evaluator eval, Environment env,
                                SyntaxSexp source, int from)
        throws FusionException
    {
        return compile(eval, env, source, from, source.size());
    }


    //========================================================================


    private static final class CompiledUndef
        implements CompiledForm
    {
        @Override
        public FusionValue doExec(Evaluator eval, Store store)
            throws FusionException
        {
            return UNDEF;
        }
    }


    private static final class CompiledBegin
        implements CompiledForm
    {
        final CompiledForm[] myBody;

        CompiledBegin(CompiledForm[] body)
        {
            myBody = body;
        }

        @Override
        public FusionValue doExec(Evaluator eval, Store store)
            throws FusionException
        {
            final int last = myBody.length - 1;
            for (int i = 0; i < last; i++)
            {
                CompiledForm form = myBody[i];
                eval.exec(store, form);
            }

            CompiledForm form = myBody[last];
            return eval.bounceTailForm(store, form);
        }
    }
}
