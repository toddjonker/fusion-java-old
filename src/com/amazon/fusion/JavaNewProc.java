// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;

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
              "is the fully-qualified Java class name. A constructor with the appropriate\n" +
              "number of FusionValue parameters will be invoked and the result returned.",
              "CLASSNAME", "ARG", DOTDOTDOT);
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        String className = checkTextArg(0, args);

        Class<?> klass = determineClass(className);

        Object result;
        if (args.length == 1)
        {
            result = callNoArgConstructor(klass);
        }
        else
        {
            // TODO FUSION-41 this should work for other types
            FusionValue[] constructorArgs =
                Arrays.copyOfRange(args, 1, args.length,
                                   FusionValue[].class);
            Constructor<?> constructor =
                determineConstructor(klass, constructorArgs);
            result = callConstructor(constructor, constructorArgs);
        }

        try
        {
            return result;
        }
        catch (ClassCastException e)
        {
            throw contractFailure("Java class isn't a FusionValue: " + className);
        }
    }


    private Class<?> determineClass(String className)
        throws FusionException
    {
        try
        {
            return Class.forName(className);
        }
        catch (ClassNotFoundException e)
        {
            throw contractFailure("Java class isn't found: " + className);
        }
    }


    private Object callNoArgConstructor(Class<?> klass)
        throws FusionException
    {
        try
        {
            return klass.newInstance();
        }
        catch (InstantiationException e)
        {
            String message =
                "Unable to instantiate Java class: " + klass +
                "; does it have a no-argument constructor?";
            throw contractFailure(message);
        }
        catch (IllegalAccessException e)
        {
            throw contractFailure("Java class isn't accessible: " + klass);
        }
    }


    private Constructor<?> determineConstructor(Class<?> klass,
                                                FusionValue[] args)
        throws FusionException
    {
        Class<?>[] parameterTypes = new Class[args.length];
        Arrays.fill(parameterTypes, FusionValue.class);

        try
        {
            return klass.getConstructor(parameterTypes);
        }
        catch (SecurityException e)
        {
            String message =
                "The Java security manager denied access to " + klass + ": " +
                e.getMessage();
            throw contractFailure(message);
        }
        catch (NoSuchMethodException e)
        {
            String message =
                "Java class doesn't have a public constructor accepting " +
                args.length + " FusionValues: " + klass;
            throw contractFailure(message);
        }
    }


    private Object callConstructor(Constructor<?> constructor, FusionValue[] args)
        throws FusionException
    {
        try
        {
            return constructor.newInstance((Object[]) args);
        }
        catch (IllegalAccessException e)
        {
            throw contractFailure("Java constructor isn't accessible: " + constructor);
        }
        catch (IllegalArgumentException e)
        {
            String message =
                "Illegal argument to Java constructor: " + constructor +
                e.getMessage();
            throw contractFailure(message);
        }
        catch (InstantiationException e)
        {
            String message =
                "Exception from Java constructor: " + constructor +
                e.getMessage();
            throw contractFailure(message);
        }
        catch (InvocationTargetException e)
        {
            String message =
                "Exception from Java constructor: " + constructor +
                e.getMessage();
            throw contractFailure(message);
        }
    }
}
