// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static java.util.Arrays.copyOfRange;
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
        super("Instantiates a new Fusion value that's implemented by a Java class. `classname`\n" +
              "is the fully-qualified Java class name. A constructor with the appropriate\n" +
              "number of `Object` arguments will be invoked and the result returned.",
              "classname", "arg", DOTDOTDOT);
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
            Object[] constructorArgs = copyOfRange(args, 1, args.length);
            Constructor<?> constructor =
                determineConstructor(klass, constructorArgs);
            result = callConstructor(constructor, constructorArgs);
        }

        result = eval.injectMaybe(result);

        if (result == null)
        {
            String message =
                "Java class cannot be injected into the Fusion runtime: " +
                className;
            throw contractFailure(message);
        }

        return result;
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
                "Unable to instantiate Java " + klass +
                "; does it have a no-argument constructor? " +
                "If an inner class, is it static?";
            throw contractFailure(message);
        }
        catch (IllegalAccessException e)
        {
            throw contractFailure("Java class isn't accessible: " + klass);
        }
    }


    private Constructor<?> determineConstructor(Class<?> klass,
                                                Object[] args)
        throws FusionException
    {
        Class<?>[] parameterTypes = new Class[args.length];
        Arrays.fill(parameterTypes, Object.class);

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
                klass + " doesn't have a public constructor accepting " +
                args.length + " Object args";
            throw contractFailure(message);
        }
    }


    private Object callConstructor(Constructor<?> constructor, Object[] args)
        throws FusionException
    {
        try
        {
            return constructor.newInstance(args);
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
