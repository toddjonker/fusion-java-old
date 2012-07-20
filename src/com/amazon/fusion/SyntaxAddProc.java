// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

class SyntaxAddProc
    extends Procedure
{
    SyntaxAddProc()
    {
        //    "                                                                               |
        super("Adds the SYNTAX object to the end of the syntax SEQUENCE.",
              "sequence", "syntax");
    }


    static void invoke(SyntaxSequence seq, SyntaxValue value)
    {
        seq.add(value);
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);
        SyntaxSequence seq = checkSyntaxSequenceArg(0, args);
        SyntaxValue value = checkSyntaxArg(1, args);

        seq.add(value);

        return args[0]; // Return the original FusionValue, no need to rewrap
    }
}
