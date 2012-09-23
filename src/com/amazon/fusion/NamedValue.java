// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
import com.amazon.ion.NullValueException;
import java.io.IOException;


/**
 *
 */
abstract class NamedValue
    extends FusionValue
{
    private String myName;


    @Override
    final String getInferredName()
    {
        return myName;
    }

    @Override
    final void inferName(String name)
    {
        if (myName == null)
        {
            myName = name;
        }
    }

    final String getEffectiveName()
    {
        String name = getInferredName();
        return (name == null ? "_" : name);
    }


    abstract void identify(Appendable out)
        throws IOException;

    /**
     * Returns the output of {@link #identify(Appendable)} as a {@link String}.
     *
     * @return not null.
     */
    String identify()
    {
        StringBuilder out = new StringBuilder();
        try
        {
            identify(out);
        }
        catch (IOException e) {}
        return out.toString();
    }


    @Override
    public final void write(Appendable out)
        throws IOException
    {
        out.append("/* ");
        identify(out);
        out.append(" */");
    }

    //========================================================================
    // Type-checking helpers

    final boolean checkBoolArg(Object arg)
        throws ContractFailure
    {
        try
        {
            IonBool iv = (IonBool) FusionValue.castToIonValueMaybe(arg);
            return iv.booleanValue();
        }
        catch (ClassCastException e) {}
        catch (NullPointerException e) {}
        catch (NullValueException e) {}

        throw contractFailure("Expected true or false: " + arg);
    }

    boolean checkBoolArg(int argNum, Object arg)
        throws ArgTypeFailure
    {
        try
        {
            IonBool iv = (IonBool) FusionValue.castToIonValueMaybe(arg);
            return iv.booleanValue();
        }
        catch (ClassCastException e) {}
        catch (NullPointerException e) {}
        catch (NullValueException e) {}

        throw new ArgTypeFailure(this, "true or false", argNum, arg);
    }


    //========================================================================
    // Error helpers

    /**
     * Returns a new {@link ContractFailure} with the given message and
     * the identification of this value. This is preferable to creating
     * the exception directly, since this method can annotate it with location
     * information.
     * <p>
     * Expected usage:
     * <pre>
     * if (somethingBadHappened)
     * {
     *   throw contractFailure("somebody screwed up");
     * }
     * </pre>
     *
     * @param message the message to render in the exception.
     * @return a new exception
     */
    ContractFailure contractFailure(String message)
    {
        return new ContractFailure(identify() + ": " + message);
    }
}
