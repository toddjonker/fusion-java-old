// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.ArrayList;

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

        ArrayList<SyntaxValue> subforms = new ArrayList<SyntaxValue>();
        subforms.add(begin);

        for (int i = from; i < size; i++)
        {
            SyntaxValue bodyForm = seq.get(i);
            subforms.add(bodyForm);
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
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        final int size = expr.size();
        if (size == 1) return UNDEF;

        FusionValue result;
        final int last = size - 1;
        for (int i = 1; i < last; i++)
        {
            SyntaxValue source = expr.get(i);
            result = eval.eval(env, source);
        }

        SyntaxValue source = expr.get(last);
        result = eval.bounceTailExpression(env, source);
        return result;
    }
}
