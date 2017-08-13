// Copyright (c) 2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Exposes binding metadata about an identifier in Fusion source code.
 * {@code BindingSite} instances form a chain from some point-of-use
 * identifier (that is, a variable reference), through applicable
 * {@code require} and {@code provide} forms, to the point of definition.
 * For the purposes of this metadata, each element in the chain is treated as
 * a binding.
 * <p>
 * Example usage:
 * Call {@link #getSourceLocation()} for the location of the binding site
 * within the file (if there is one).
 * <p>
 * If there isn't, check if the binding information was obtained from an
 * imported binding using {@link #isImportSite()} and, if it was, then use
 * {@link #getExportSite()} to get the {@link BindingSite}
 * of the provide site for the imported binding.
 * <p>
 * This sequence would then be repeated for the identifier at the provide site
 * which is another variable reference which might or might not be imported.
 * <p>
 * Calling {@link #target()} will get of the {@link BindingSite} of the
 * identifiers original binding site, wherever that may be.
 */
public class BindingSite
{
    private final SourceLocation srcLoc;
    private final BindingSite    targetSite;

    private BindingSite(SourceLocation sourceLocation,
                        BindingSite    targetInformation)
    {
        srcLoc = sourceLocation;
        targetSite = targetInformation;
    }


    /**
     * Returns the location of this site.
     *
     * @return null if called on a binding with no local binding reference.
     * This happens when there is no explicit identifier given during import.
     */
    public SourceLocation getSourceLocation()
    {
        return srcLoc;
    }


    /**
     * Gets the site where an imported binding was exported.
     *
     * Example:
     * <pre>
     * (module mod "/fusion"
     *   (define foo 7)
     *   (provide foo)
     *
     * (require (only_in mod foo))
     * foo
     * </pre>
     *
     * The trailing top level {@code foo} has an imported binding. Calling this
     * method on its {@link BindingSite} will return the site of the
     * {@code (provide foo)} in {@code mod}.
     * Calling {@link #getSourceLocation()} on the former site would return the
     * location of the {@code foo} reference within {@code only_in}.
     *
     * @return null unless this {@link #isImportSite()}.
     */
    public BindingSite getExportSite()
    {
        return null;
    }


    /**
     * Indicates whether this site is where a binding was {@code require}d or
     * imported via a language declaration.
     */
    public boolean isImportSite()
    {
        return false;
    }


    /**
     * Gets the BindingSite of the Binding targeted by this Binding.
     * This jumps across modules, renames, and files to get to the original
     * definition site.
     *
     * @return not null. Returns itself iff the Binding was original.
     */
    public BindingSite target()
    {
        if (targetSite == null)
        {
            return this;
        }
        return targetSite;
    }


    //=========================================================================


    private static final class ImportSite
        extends BindingSite
    {
        private final BindingSite myExportSite;

        private ImportSite(SourceLocation importLocation,
                           BindingSite    exportSite,
                           BindingSite    targetSite)
        {
            super(importLocation, targetSite);
            myExportSite = exportSite;
        }

        @Override
        public BindingSite getExportSite()
        {
            return myExportSite;
        }

        @Override
        public boolean isImportSite()
        {
            return true;
        }
    }


    //=========================================================================
    // Factory Methods


    /**
     * @param bindingLoc the location of the identifier where the binding
     * was declared.
     */
    static BindingSite makeLocalBindingSite(SourceLocation bindingLoc)
    {
        return new BindingSite(bindingLoc, null);
    }

    /**
     * Creates a site for a namespace-level definition.
     *
     * @param definitionLoc the location of the identifier where the binding
     * was declared.
     */
    static BindingSite makeDefineBindingSite(SourceLocation definitionLoc)
    {
        return new BindingSite(definitionLoc, null);
    }

    static BindingSite makeExportBindingSite(SourceLocation srcLoc,
                                             BindingSite    bindingInfo)
    {
        return new BindingSite(srcLoc, bindingInfo);
    }

    static BindingSite makeBindingSite(SourceLocation srcLoc,
                                       BindingSite    bindingInfo)
    {
        return new BindingSite(srcLoc, bindingInfo);
    }


    static BindingSite makeImportBindingSite(SourceLocation importLocation,
                                             BindingSite    exportBindingSite,
                                             BindingSite    targetSite)
    {
        return new ImportSite(importLocation, exportBindingSite, targetSite);
    }
}
