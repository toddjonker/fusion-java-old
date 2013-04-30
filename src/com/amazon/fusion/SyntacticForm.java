// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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
    protected final String myBodyPattern;
    protected final String myDoc;
    private final BindingDoc myDocs;

    SyntacticForm(String bodyPattern, String doc)
    {
        myBodyPattern = bodyPattern;
        myDoc = doc;

        myDocs = new BindingDoc(null, Kind.SYNTAX, myBodyPattern, myDoc);
    }


    @Override
    final void nameInferred(String name)
    {
        myDocs.setName(name);
    }


    abstract SyntaxValue expand(Expander expander, Environment env,
                                SyntaxSexp stx)
        throws FusionException;


    /** Expand elements [1...] from a sexp. */
    final SyntaxValue expandArgs(Expander expander, Environment env,
                                 SyntaxSexp stx)
        throws FusionException
    {
        int size = stx.size();

        SyntaxValue[] expandedChildren = new SyntaxValue[size];
        expandedChildren[0] = stx.get(0);

        for (int i = 1; i < size; i++)
        {
            SyntaxValue subform = stx.get(i);
            expandedChildren[i] = expander.expandExpression(env, subform);
        }
        return SyntaxSexp.make(expander, stx.getLocation(), expandedChildren);
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

    final SyntaxChecker check(SyntaxSexp form)
    {
        return new SyntaxChecker(getInferredName(), form);
    }
}
