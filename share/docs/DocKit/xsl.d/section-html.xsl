<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:import href="docbook-xsl/html/chunkfast.xsl"/>
<xsl:import href="programlisting-format-html.xsl"/>
<xsl:import href="docbook-xsl/html/highlight.xsl"/>
<xsl:import href="extensions.xsl"/>

<xsl:param name="generate.section.toc.level" select="8"/>
<xsl:param name="bridgehead.in.toc" select="1"/>
<xsl:param name="section.autolabel" select="1"/>
<xsl:param name="section.label.includes.component.label" select="1"/>
<xsl:param name="use.id.as.filename" select="1"/>
<xsl:param name="chunk.section.depth" select="2"/>

<xsl:param name="chunk.quietly" select="1"/>
<xsl:param name="chunk.first.sections" select="1"/>
<xsl:param name="chunk.fast" select="1"/>

<xsl:param name="chunker.output.encoding" select="'utf-8'"/>

<xsl:template match="remark" />

<xsl:template name="user.head.content">
  <meta name="date">  
    <xsl:attribute name="content">__meta_html_builddate__</xsl:attribute>
  </meta>
</xsl:template>

</xsl:stylesheet>
