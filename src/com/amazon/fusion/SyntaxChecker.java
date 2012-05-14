// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;

/**
 * Helper class for checking syntactic forms.
 */
final class SyntaxChecker
{
    private final String myFormName;
    private final IonSexp myForm;

    SyntaxChecker(String formName, IonSexp form)
    {
        myFormName = formName;
        myForm = form;
    }


    //========================================================================

    final SyntaxFailure failure(String message)
    {
        return new SyntaxFailure(myFormName, message, myForm);
    }


    final void arityExact(int count)
        throws SyntaxFailure
    {
        if (myForm.size() != count)
        {
            throw failure("expected " + count + " subforms");
        }
    }


    final IonValue requiredForm(String expectation, int argNum)
        throws SyntaxFailure
    {
        try
        {
            return myForm.get(argNum);
        }
        catch (IndexOutOfBoundsException e) {}

        throw failure("expected " + expectation);
    }


    final IonString requiredString(String expectation, int argNum)
        throws SyntaxFailure
    {
        IonValue form = requiredForm(expectation, argNum);
        return checkSyntax(IonString.class, expectation,
                           false /* nullable */, form);
    }


    final String requiredNonEmptyString(String expectation, int argNum)
        throws SyntaxFailure
    {
        IonString sym = requiredString(expectation, argNum);
        // TODO check emptyness
        return sym.stringValue();
    }



    final IonSymbol requiredSymbol(String expectation, int argNum)
        throws SyntaxFailure
    {
        IonValue form = requiredForm(expectation, argNum);
        return checkSyntax(IonSymbol.class, expectation,
                           false /* nullable */, form);
    }

    final String requiredNonEmptySymbol(String expectation, int argNum)
        throws SyntaxFailure
    {
        IonSymbol sym = requiredSymbol(expectation, argNum);
        // TODO check emptyness
        return sym.stringValue();
    }


    final <T extends IonValue> T checkSyntax(Class<T> klass,
                                             String expectation,
                                             boolean nullable,
                                             IonValue form)
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

        throw new SyntaxFailure(myFormName,
                                "expected " + expectation, form);
    }
}
