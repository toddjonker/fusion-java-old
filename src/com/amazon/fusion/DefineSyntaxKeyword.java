// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

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
    FusionValue invoke(Evaluator eval, Environment env, IonSexp stx)
        throws FusionException
    {
        SyntaxChecker check = new SyntaxChecker(getInferredName(), stx);
        check.arityExact(3);

        String keyword = check.requiredNonEmptySymbol("identifier", 1);

        IonValue valueStx = stx.get(2);
        FusionValue xform = eval.eval(env, valueStx);

        // TODO check result type
        Procedure xformProc = (Procedure) xform;
        MacroTransformer macro = new MacroTransformer(xformProc);

        Namespace ns = env.namespace();
        ns.bind(keyword, macro);

        return UNDEF;
    }
}
