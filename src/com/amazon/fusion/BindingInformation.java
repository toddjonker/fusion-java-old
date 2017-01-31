// Copyright (c) 2017-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * For use with the Fusion plugins.
 * Provides an interface to access the necessary information from a Binding.
 *
 * Example usage:
 * Call {@link #getSourceLocation()} for the location of the binding site
 * within the file (if there is one).
 *
 * If there isn't, check if the binding information was obtained from an
 * imported binding using {@link #isRequiredBinding()} and, if it was, then use
 * {@link #getModuleBindingInformation()} to get the {@link BindingInformation}
 * of the provide site for the imported binding.
 *
 * This sequence would then be repeated for the identifier at the provide site
 * which is another variable reference which might or might not be imported.
 *
 * Calling {@link #target()} will get of the {@link BindingInformation} of the
 * identifiers original binding site, wherever that may be.
 */
public class BindingInformation
{
    private final SourceLocation     srcLoc;
    private final BindingInformation targetInfo;

    private BindingInformation(SourceLocation     sourceLocation,
                               BindingInformation targetInformation)
    {
        srcLoc = sourceLocation;
        targetInfo = targetInformation;
    }


    /**
     * Returns the SourceLocation of the binding site within the file.
     *
     * @return null if called on a binding with no local binding reference.
     * This happens when there is no explicit identifier given during import.
     */
    public SourceLocation getSourceLocation()
    {
        return srcLoc;
    }


    /**
     * Gets the BindingInformation of the module defined binding
     * of a "require"d in binding.
     *
     * Example:
     * (module mod "/fusion"
     *   (define foo 7)
     *   (provide foo)
     *
     * (require (only_in mod foo))
     * foo
     *
     * The top level "foo" is a "require"d in binding. Calling this method on
     * its BindingInformation will return the BindingInformation of the "foo"
     * at the provide site within the module. This differs from
     * {@link #getSourceLocation()} which would return the location of the "foo"
     * within the only_in location.
     *
     * @return null if not called on a "required"d binding.
     */
    public BindingInformation getModuleBindingInformation()
    {
        return null;
    }


    public boolean isRequiredBinding()
    {
        return false;
    }


    /**
     * Gets the BindingInformation of the Binding targeted by this Binding.
     * This jumps across modules, renames, and files to get to the original
     * definition site.
     *
     * @return not null. Returns itself iff the Binding was original.
     */
    public BindingInformation target()
    {
        if (targetInfo == null)
        {
            return this;
        }
        return targetInfo;
    }


    //=========================================================================


    private static class RequiredBindingInformation
        extends BindingInformation
    {
        private final BindingInformation moduleInfo;

        private RequiredBindingInformation(SourceLocation     sourceLocation,
                                           BindingInformation moduleBindingInfo,
                                           BindingInformation targetInformation)
        {
            super(sourceLocation, targetInformation);
            moduleInfo = moduleBindingInfo;
        }

        @Override
        public BindingInformation getModuleBindingInformation()
        {
            return moduleInfo;
        }

        @Override
        public boolean isRequiredBinding()
        {
            return true;
        }
    }


    //=========================================================================
    // Factory Methods


    static BindingInformation makeBindingInfo(SourceLocation     srcLoc,
                                              BindingInformation bindingInfo)
    {
        return new BindingInformation(srcLoc, bindingInfo);
    }


    static BindingInformation makeRequiredBindingInfo(SourceLocation     srcLoc,
                                                      BindingInformation moduleBindingInfo,
                                                      BindingInformation targetInfo)
    {
        return new RequiredBindingInformation(srcLoc,
                                              moduleBindingInfo,
                                              targetInfo);
    }
}
