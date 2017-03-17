// Copyright (c) 2012-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code begin} syntactic form.
 */
final class BeginForm
    extends SyntacticForm
{
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
    CompiledForm compile(Compiler comp, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        return comp.compileBegin(env, stx, 1, stx.size());
    }
}
