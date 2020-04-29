// Copyright (c) 2011-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.junit;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import org.junit.runner.Runner;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.Parameterized;
import org.junit.runners.Suite;
import org.junit.runners.model.FrameworkField;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.Statement;
import org.junit.runners.model.TestClass;

/**
 * A JUnit 4 {@link Runner} that injects one or more JavaBeans properties of
 * the test fixture with a set of configured values.  This approach is similar
 * to {@link Parameterized} but utilizes setter injection instead of using
 * constructors, so its easier to reuse the injection via inheritance.
 * <p>
 * <b>This class is forked from IonJava!</b>
 */
public class Injected
    extends Suite
{

    /**
     * Annotation for a public static field which provides values to be
     * injected into the fixture. The {@code value} element of this annotation
     * must be the name of a writable property of the fixture.
     */
    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.FIELD)
    public @interface Inject
    {
        /**
         * The name of the property to inject.
         */
        String value();
    }


    private static final class Dimension
    {
        String property;
        PropertyDescriptor descriptor;
        Object[] values;
    }


    private static class InjectedRunner
        extends BlockJUnit4ClassRunner
    {
        private final Dimension[] myDimensions;
        private final int[] myValueIndices;
        private String myName;

        /**
         * @param klass
         * @throws InitializationError
         */
        public InjectedRunner(Class<?> klass,
                              Dimension[] dimensions,
                              int... indices)
            throws InitializationError
        {
            super(klass);

            assert dimensions.length == indices.length;

            myDimensions = dimensions;
            myValueIndices = indices;
        }

        @Override
        public Object createTest()
            throws Exception
        {
            Object test = getTestClass().getOnlyConstructor().newInstance();
            for (int i = 0; i < myDimensions.length; i++)
            {
                inject(test, myDimensions[i], myValueIndices[i]);
            }
            return test;
        }

        private void inject(Object target, Dimension dimension, int valueIndex)
            throws Exception
        {
            Method method = dimension.descriptor.getWriteMethod();
            Object value  = dimension.values[valueIndex];
            method.invoke(target, value);
        }

        @Override
        protected synchronized String getName()
        {
            if (myName == null)
            {
                StringBuilder buf = new StringBuilder("[");
                for (int i = 0; i < myDimensions.length; i++)
                {
                    if (i != 0) buf.append(',');

                    Dimension dim = myDimensions[i];
                    int valueIndex =  myValueIndices[i];
                    buf.append(dim.values[valueIndex]);
                }
                buf.append(']');
                myName = buf.toString();
            }
            return myName;
        }

        @Override
        protected String testName(FrameworkMethod method)
        {
            // Eclipse (Helios) can't display results properly if the names
            // are not unique.
            return method.getName() + getName();
        }

        @Override
        protected void validateConstructor(List<Throwable> errors)
        {
            validateOnlyOneConstructor(errors);
        }

        @Override
        protected Statement classBlock(RunNotifier notifier)
        {
            return childrenInvoker(notifier);
        }
    }



    /**
     * Only called reflectively. Do not use programmatically.
     */
    public Injected(Class<?> klass)
        throws Throwable
    {
        super(klass, new ArrayList<Runner>());

        Dimension[] dimensions = findDimensions();
        assert dimensions.length != 0;

        fanout(klass, dimensions, new int[dimensions.length], 0);
    }


    private void fanout(Class<?> klass,
                        Dimension[] dimensions,
                        int[] valueIndices,
                        int dimensionIndex)
        throws InitializationError
    {
        assert dimensions.length == valueIndices.length;

        if (dimensionIndex == dimensions.length)
        {
            InjectedRunner runner =
                new InjectedRunner(klass, dimensions, valueIndices);
            getChildren().add(runner);
        }
        else
        {
            Dimension dim = dimensions[dimensionIndex];
            int width = dim.values.length;
            for (int i = 0; i < width; i++)
            {
                int[] childIndexes = valueIndices.clone();
                childIndexes[dimensionIndex] = i;
                fanout(klass, dimensions, childIndexes, dimensionIndex + 1);
            }
        }
    }


    private Dimension[] findDimensions()
        throws Throwable
    {
        TestClass testClass = getTestClass();

        List<FrameworkField> fields =
            testClass.getAnnotatedFields(Inject.class);
        if (fields.isEmpty())
        {
            throw new Exception("No fields of " + testClass.getName()
                                + " have the @Inject annotation");
        }

        BeanInfo beanInfo = Introspector.getBeanInfo(testClass.getJavaClass());
        PropertyDescriptor[] descriptors = beanInfo.getPropertyDescriptors();

        Dimension[] dimensions = new Dimension[fields.size()];

        int i = 0;
        for (FrameworkField field : fields)
        {
            int modifiers = field.getField().getModifiers();
            if (! Modifier.isPublic(modifiers) || ! Modifier.isStatic(modifiers))
            {
                throw new Exception("@Inject " + testClass.getName() + '.'
                                    + field.getField().getName()
                                    + " must be public static");
            }

            Dimension dim = new Dimension();
            dim.property = field.getField().getAnnotation(Inject.class).value();
            dim.descriptor = findDescriptor(testClass, descriptors, field, dim.property);
            dim.values = (Object[]) field.get(null);
            dimensions[i++] = dim;
        }

        return dimensions;
    }


    private PropertyDescriptor findDescriptor(TestClass testClass,
                                              PropertyDescriptor[] descriptors,
                                              FrameworkField field,
                                              String name)
        throws Exception
    {
        for (PropertyDescriptor d : descriptors)
        {
            if (d.getName().equals(name))
            {
                if (d.getWriteMethod() == null) break;  // To throw error
                return d;
            }
        }

        throw new Exception("@Inject value '" + name
                            + "' doesn't match a writeable property near "
                            + testClass.getName() + '.'
                            + field.getField().getName());
    }
}
