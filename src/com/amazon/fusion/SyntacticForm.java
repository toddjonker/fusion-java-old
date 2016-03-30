// Copyright (c) 2012-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.BindingDoc.Kind;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;

/**
 * Base class for syntactic forms.
 */
abstract class SyntacticForm
    extends NamedValue
{
    private final BindingDoc myDocs;

    public SyntacticForm()
    {
        myDocs = null;

    }

    @Deprecated
    SyntacticForm(String bodyPattern, String doc)
    {
        myDocs = new BindingDoc(null, Kind.SYNTAX, bodyPattern, doc);
    }


    @Override
    final void nameInferred(String name)
    {
        if (myDocs != null) myDocs.setName(name);
    }


    abstract SyntaxValue expand(Expander expander, Environment env,
                                SyntaxSexp stx)
        throws FusionException;


    /** Expand elements [1...] from a sexp. */
    final SyntaxValue expandArgs(Expander expander, Environment env,
                                 SyntaxSexp stx)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

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


    abstract CompiledForm compile(Evaluator eval, Environment env,
                                  SyntaxSexp stx)
        throws FusionException;


    @Override
    final void identify(Appendable out)
        throws IOException
    {
        String name = getInferredName();
        if (name == null)
        {
            out.append("anonymous syntax");
        }
        else
        {
            out.append("syntax ");
            IonTextUtils.printQuotedSymbol(out, name);
        }
    }


    @Override
    BindingDoc document()
    {
        return myDocs;
    }


    //========================================================================
    // Type-checking helpers

    final SyntaxChecker check(Evaluator eval, SyntaxSexp form)
    {
        return new SyntaxChecker(eval, getInferredName(), form);
    }

    final SyntaxChecker check(Expander expander, SyntaxSexp form)
    {
        return check(expander.getEvaluator(), form);
    }
}
