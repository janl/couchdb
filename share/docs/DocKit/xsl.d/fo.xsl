<?xml version="1.0"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:import href="docbook-xsl/fo/docbook.xsl"/>
  <xsl:import href="docbook-xsl/fo/highlight.xsl"/>
  <xsl:import href="extensions.xsl"/>

  <xsl:param name="fop1.extensions" select="1"/>  
  <xsl:param name="bookmarks.collapse" select="1"/>  

  <xsl:param name="section.autolabel" select="1"/>
  <xsl:param name="section.label.includes.component.label" select="1"/>

  <xsl:param name="page.margin.inner" select="'0.5in'"/>
  <xsl:param name="page.margin.outer" select="'0.5in'"/>
  <xsl:param name="draft.mode" select="no"/>

  <xsl:param name="body.font.family">Times</xsl:param>
  <xsl:param name="body.font.master">10</xsl:param>

  <xsl:param name="title.font.family">Helvetica</xsl:param>

  <xsl:param name="monospace.font.family">Courier</xsl:param>

  <xsl:param name="ulink.show">0</xsl:param>

  <xsl:attribute-set name="component.title.properties">
    <xsl:attribute name="font-size">18pt</xsl:attribute>
    <xsl:attribute name="font-weight">normal</xsl:attribute>
    <xsl:attribute name="color">#a30a0a</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.title.level1.properties">
    <xsl:attribute name="font-size">16pt</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="color">#a30a0a</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.title.level2.properties">
    <xsl:attribute name="font-size">14pt</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="color">#a30a0a</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.title.level3.properties">
    <xsl:attribute name="font-size">12pt</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="color">#a30a0a</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.title.level4.properties">
    <xsl:attribute name="font-size">11pt</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="color">#a30a0a</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.title.level5.properties">
    <xsl:attribute name="font-size">10pt</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="color">#a30a0a</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="section.title.level6.properties">
    <xsl:attribute name="font-size">10pt</xsl:attribute>
    <xsl:attribute name="font-weight">normal</xsl:attribute>
    <xsl:attribute name="color">#a30a0a</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="xref.properties">
    <xsl:attribute name="color">#0A72A3</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="monospace.properties">
    <xsl:attribute name="color">#026789</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="admonition.title.properties">
    <xsl:attribute name="space-before.minimum">0em</xsl:attribute>
    <xsl:attribute name="space-before.optimum">0em</xsl:attribute>
    <xsl:attribute name="space-before.maximum">0em</xsl:attribute>
    <xsl:attribute name="color">#a30a0a</xsl:attribute>
    <xsl:attribute name="font-family">
      <xsl:value-of select="$title.font.family"/>
    </xsl:attribute>
    <xsl:attribute name="font-size">10pt</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
    <xsl:attribute name="margin-left">0.25in</xsl:attribute>
    <xsl:attribute name="margin-right">0.25in</xsl:attribute>
    <xsl:attribute name="padding-left">0.1in</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="admonition.properties">
    <xsl:attribute name="space-before.minimum">0em</xsl:attribute>
    <xsl:attribute name="space-before.optimum">0em</xsl:attribute>
    <xsl:attribute name="space-before.maximum">0em</xsl:attribute>
    <xsl:attribute name="margin-left">0.25in</xsl:attribute>
    <xsl:attribute name="margin-right">0.25in</xsl:attribute>
    <xsl:attribute name="border-left">5px solid black</xsl:attribute>
    <xsl:attribute name="padding-left">0.1in</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="formal.title.properties">
    <xsl:attribute name="space-before.minimum">0.4em</xsl:attribute>
    <xsl:attribute name="space-before.optimum">0.6em</xsl:attribute>
    <xsl:attribute name="space-before.maximum">0.8em</xsl:attribute>
    <xsl:attribute name="font-family">
      <xsl:value-of select="$title.font.family"/>
    </xsl:attribute>
    <xsl:attribute name="font-size">10pt</xsl:attribute>
    <xsl:attribute name="font-weight">bold</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="monospace.verbatim.properties">
    <xsl:attribute name="background-color">#02354d</xsl:attribute>
    <xsl:attribute name="color">#ffffff</xsl:attribute>
    <xsl:attribute name="padding">1px</xsl:attribute>
    <xsl:attribute name="font-size">
      <xsl:value-of select="$body.font.master * 0.7"/>
      <xsl:text>pt</xsl:text>
    </xsl:attribute>
    <xsl:attribute name="keep-together.within-column">always</xsl:attribute>
  </xsl:attribute-set>

  <xsl:attribute-set name="root.properties">
    <xsl:attribute name="text-align">
      <xsl:text>left</xsl:text>
    </xsl:attribute>
  </xsl:attribute-set>

  <!-- strip remark elements -->
  <xsl:template match="remark" />

</xsl:stylesheet>
