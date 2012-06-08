// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 *
 */
final class JavaNewProc
    extends Procedure
{
    JavaNewProc()
    {
        //    "                                                                               |
        super("Instantiates a new Fusion value that's implemented by a Java class. CLASSNAME\n" +
              "is the fully-qualified Java class name; its no-argument constructor will be\n" +
              "invoked and the result returned.",
              "CLASSNAME");
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);
        String className = checkTextArg(0, args);

        try
        {
            Class<?> c = Class.forName(className);
            Object o = c.newInstance();
            return (FusionValue) o;
        }
        catch (ClassNotFoundException e)
        {
            throw contractFailure("Java class isn't found: " + className);
        }
        catch (InstantiationException e)
        {
            throw contractFailure("Error constructing Java class: " + className);
        }
        catch (IllegalAccessException e)
        {
            throw contractFailure("Java class isn't accessible: " + className);
        }
        catch (ClassCastException e)
        {
            throw contractFailure("Java class isn't a FusionValue: " + className);
        }
    }
}
