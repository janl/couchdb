<?xml version="1.0" encoding="utf-8"?>

<!--
        Standard single HTML file output
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="docbook-xsl/html/docbook.xsl"/>
<xsl:import href="programlisting-format-html.xsl"/>
<xsl:import href="docbook-xsl/html/highlight.xsl"/>
<xsl:import href="extensions.xsl"/>
<xsl:output method="html" encoding="utf-8" indent="no"/>

<xsl:param name="section.autolabel" select="1"/>
<xsl:param name="section.label.includes.component.label" select="1"/>

<!-- sections get own TOC if they have subsections -->
<xsl:param name="generate.section.toc.level" select="2"/>

<!-- strip remark elements -->
<xsl:template match="remark" />

</xsl:stylesheet>
