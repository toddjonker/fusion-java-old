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

    SyntacticForm(String bodyPattern, String doc)
    {
        myBodyPattern = bodyPattern;
        myDoc = doc;
    }


    SyntaxValue expand(Evaluator eval, Expander ctx, Environment env,
                       SyntaxSexp source)
        throws FusionException
    {
        int size = source.size();

        SyntaxValue[] expandedChildren = new SyntaxValue[size];
        expandedChildren[0] = source.get(0);

        for (int i = 1; i < size; i++)
        {
            SyntaxValue subform = source.get(i);
            expandedChildren[i] = eval.expand(ctx, env, subform);
        }
        return SyntaxSexp.make(source.getLocation(), expandedChildren);
    }


    abstract CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
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
        String name = getDocumentedName();

        StringBuilder buf = new StringBuilder();
        buf.append('(');
        buf.append(name);

        if (myBodyPattern != null)
        {
            buf.append(' ');
            buf.append(myBodyPattern);
        }
        buf.append(')');
        String usage = buf.toString();

        return new BindingDoc(name, Kind.SYNTAX, usage, myDoc);
    }


    //========================================================================
    // Type-checking helpers

    final SyntaxChecker check(SyntaxSexp form)
    {
        return new SyntaxChecker(getInferredName(), form);
    }


    final SyntaxValue requiredForm(String expectation,
                                   int argNum,
                                   SyntaxSequence parent)
        throws SyntaxFailure
    {
        try
        {
            return parent.get(argNum);
        }
        catch (IndexOutOfBoundsException e) {}

        throw new SyntaxFailure(identify(),
                                "expected " + expectation,
                                parent);
    }
}
