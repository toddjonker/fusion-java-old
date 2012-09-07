// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;

/**
 * Base class for syntactic keywords.
 */
abstract class KeywordValue
    extends NamedValue
{
    protected final String myBodyPattern;
    protected final String myDoc;

    KeywordValue(String bodyPattern, String doc)
    {
        myBodyPattern = bodyPattern;
        myDoc = doc;
    }


    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        int size = source.size();

        SyntaxValue[] expandedChildren = new SyntaxValue[size];
        expandedChildren[0] = source.get(0);

        for (int i = 1; i < size; i++)
        {
            SyntaxValue subform = source.get(i);
            expandedChildren[i] = subform.prepare(eval, env);
        }
        return SyntaxSexp.make(source.getLocation(), expandedChildren);
    }


    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        return source;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        CompiledForm compiled = compile(eval, env, expr);
        return eval.bounceTailForm(env, compiled);
    }


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
            out.append("keyword ");
            IonTextUtils.printQuotedSymbol(out, name);
        }
    }


    @Override
    void displayHelp(Appendable out)
        throws IOException
    {
        out.append("[SYNTAX]  (");
        out.append(getEffectiveName());
        if (myBodyPattern != null)
        {
            out.append(' ');
            out.append(myBodyPattern);
        }
        out.append(")\n\n");
        out.append(myDoc);
        out.append('\n');
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

        throw new SyntaxFailure(getEffectiveName(),
                                "expected " + expectation,
                                parent);
    }


    final SyntaxSymbol requiredSymbol(String expectation,
                                      int argNum,
                                      SyntaxSequence parent)
        throws SyntaxFailure
    {
        SyntaxValue form = requiredForm(expectation, argNum, parent);
        return checkSyntax(SyntaxSymbol.class, expectation,
                           false /* nullable */, form);
    }

    final SyntaxSymbol requiredSymbol(String expectation, SyntaxValue arg)
        throws SyntaxFailure
    {
        return checkSyntax(SyntaxSymbol.class, expectation,
                           false /* nullable */, arg);
    }


    final SyntaxSequence requiredSequence(String expectation,
                                          int argNum,
                                          SyntaxSequence parent)
        throws SyntaxFailure
    {
        SyntaxValue form = requiredForm(expectation, argNum, parent);
        return checkSyntax(SyntaxSequence.class, expectation,
                           false /* nullable */, form);
    }

    final SyntaxSequence requiredSequence(String expectation, SyntaxValue arg)
        throws SyntaxFailure
    {
        return checkSyntax(SyntaxSequence.class, expectation,
                           false /* nullable */, arg);
    }


    final SyntaxSexp requiredSexp(String expectation,
                                  int argNum,
                                  SyntaxSequence parent)
        throws SyntaxFailure
    {
        SyntaxValue form = requiredForm(expectation, argNum, parent);
        return checkSyntax(SyntaxSexp.class, expectation,
                           false /* nullable */, form);
    }

    final SyntaxSexp requiredSexp(String expectation, SyntaxValue arg)
        throws SyntaxFailure
    {
        return checkSyntax(SyntaxSexp.class, expectation,
                           false /* nullable */, arg);
    }


    final <T extends SyntaxValue> T checkSyntax(Class<T> klass,
                                               String expectation,
                                               boolean nullable,
                                               SyntaxValue form)
        throws SyntaxFailure
    {
        try
        {
            if (nullable || ! form.isNullValue())
            {
                return klass.cast(form);
            }
        }
        catch (ClassCastException e) {}

        throw new SyntaxFailure(this.getEffectiveName(),
                                "expected " + expectation, form);
    }
}
