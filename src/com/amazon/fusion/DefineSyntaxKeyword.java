// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class DefineSyntaxKeyword
    extends KeywordValue
{
    DefineSyntaxKeyword()
    {
        //    "                                                                               |
        super("ID XFORM",
              "Defines a syntax keyword ID with the given syntax transformer XFORM. The\n" +
              "transformer must be a procedure that accepts an Ion value and returns and Ion\n" +
              "value.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxChecker check = new SyntaxChecker(getInferredName(), stx);
        check.arityExact(3);

        String keyword = check.requiredNonEmptySymbol("identifier", 1);

        SyntaxValue valueStx = stx.get(2);
        FusionValue xform = eval.eval(env, valueStx);

        KeywordValue result;
        if (xform instanceof KeywordValue)
        {
            result = (KeywordValue) xform;
        }
        else if (xform instanceof Procedure)
        {
            Procedure xformProc = (Procedure) xform;
            result = new MacroTransformer(xformProc);
        }
        else
        {
            String message =
                "value is not a transformer: " + displayToString(xform);
            throw check.failure(message);
        }

        Namespace ns = env.namespace();
        ns.bind(keyword, result);

        return UNDEF;
    }
}
