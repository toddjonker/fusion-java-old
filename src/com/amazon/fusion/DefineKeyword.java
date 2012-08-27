// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class DefineKeyword
    extends KeywordValue
{
    DefineKeyword()
    {
        //    "                                                                               |
        super("VAR VALUE",
              "Defines a top-level variable VAR with the given VALUE.");
    }


    static String boundName(Evaluator eval, Environment env, SyntaxSexp stx)
        throws SyntaxFailure
    {
        // TODO error checking
        SyntaxSymbol name = (SyntaxSymbol) stx.get(1);
        return name.stringValue();
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker check = new SyntaxChecker(getInferredName(), source);
        check.arityExact(3);
        SyntaxSymbol name = (SyntaxSymbol) source.get(1); // TODO type-check
        SyntaxValue valueStx = source.get(2);

        Namespace ns = env.namespace();
        if (ns.lookup(name.stringValue()) == null)
        {
            // If at module top-level, this has already been done.
            // TODO we should know the context where this is happening...
            ns.predefine(name.stringValue());
        }

        valueStx = valueStx.prepare(eval, env);

        source = SyntaxSexp.make(source.getLocation(),
                                 source.get(0), name, valueStx);
        return source;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxSymbol name = (SyntaxSymbol) stx.get(1);
        SyntaxValue valueStx = stx.get(2);

        FusionValue fusionValue = eval.eval(env, valueStx);

        Namespace ns = env.namespace();
        ns.bind(name.stringValue(), fusionValue);

        return fusionValue;
    }
}
