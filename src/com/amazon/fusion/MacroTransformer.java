// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 *
 */
final class MacroTransformer
    extends KeywordValue
{
    private final Procedure myTransformer;


    MacroTransformer(Procedure transformer)
    {
        super(null, null); // TODO
        myTransformer = transformer;
    }


    SyntaxValue expandOnce(Evaluator eval, Environment env, SyntaxSexp expr)
        throws SyntaxFailure
    {
        try
        {
            // TODO error check
            return (SyntaxValue) eval.applyNonTail(myTransformer, expr);
        }
        catch (SyntaxFailure e)
        {
            e.addContext(expr);
            throw e;
        }
        catch (FusionException e)
        {
            String message =
                "Error expanding macro: " + e.getMessage();
            throw new SyntaxFailure(getInferredName(), message, expr);
        }
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp expr)
        throws SyntaxFailure
    {
        SyntaxValue expanded = expandOnce(eval, env, expr);

        return expanded.prepare(eval, env);
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        Object resultFv = eval.applyNonTail(myTransformer, expr);
        // TODO error check
        return eval.bounceTailExpression(env, resultFv);
    }
}
