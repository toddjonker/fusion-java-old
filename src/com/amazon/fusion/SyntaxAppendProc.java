// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.ArrayList;

final class SyntaxAppendProc
    extends Procedure
{
    SyntaxAppendProc()
    {
        //    "                                                                               |
        super("Returns a new syntax sexp that combines the HEAD and TAIL sequences.",
              "head", "tail");
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);
        SyntaxSequence head = checkSyntaxSequenceArg(0, args);
        SyntaxSequence tail = checkSyntaxSequenceArg(1, args);

        ArrayList<SyntaxValue> children =
            new ArrayList<SyntaxValue>(head.size() + tail.size());
        children.addAll(head.myChildren);
        children.addAll(tail.myChildren);

        SyntaxSexp result = SyntaxSexp.make(/* location */ null, children);
        return result;
    }
}
