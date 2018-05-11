
module Termonad.XML where

-- TODO: A number of widgets have different places where a child can be added
-- (e.g. tabs vs. page content in notebooks). This can be reflected in a UI
-- definition by specifying the “type” attribute on a <child> The possible
-- values for the “type” attribute are described in the sections describing the
-- widget-specific portions of UI definitions.

interfaceDoc :: Document
interfaceDoc =
  [xmlRaw|
    <?xml version="1.0" encoding="UTF-8"?>
    <interface>
      <!-- interface-requires gtk+ 3.8 -->
      <object id="appWin" class="GtkApplicationWindow">
        <property name="title" translatable="yes">Example Application</property>
        <property name="default-width">600</property>
        <property name="default-height">400</property>
        <child>
          <object class="GtkBox" id="content_box">
            <property name="visible">True</property>
            <property name="orientation">vertical</property>
            <child>
              <object class="GtkHeaderBar" id="header">
                <property name="visible">True</property>
                <child type="title">
                  <object class="GtkStackSwitcher" id="tabs">
                    <property name="visible">True</property>
                    <property name="stack">stack</property>
                    <style>
                      <class name="my-special-stackswitcher-class"/>
                      <class name="dark-stackswitcher"/>
                    </style>
                  </object>
                </child>
              </object>
            </child>
            <child>
              <object class="GtkStack" id="stack">
                <property name="visible">True</property>
              </object>
            </child>
            <child>
              <object class="GtkImage" id="image1">
                <property name="visible">True</property>
                <property name="stock">gtk-apply</property>
                <property name="icon_size">4</property>
                <property name="xalign">0.5</property>
                <property name="yalign">0.5</property>
                <property name="xpad">0</property>
                <property name="ypad">0</property>
              </object>
              <packing>
                <property name="padding">0</property>
                <property name="expand">False</property>
                <property name="fill">False</property>
              </packing>
            </child>
            <child>
              <object class="GtkButton" id="button1">
                <property name="visible">True</property>
                <property name="can_focus">True</property>
                <property name="relief">GTK_RELIEF_NORMAL</property>
              </object>
            </child>
          </object>
        </child>
      </object>
    </interface>
   |]

interfaceText :: Text
interfaceText = toStrict $ renderText def interfaceDoc

menuDoc :: Document
menuDoc =
  [xmlRaw|
    <?xml version="1.0"?>
    <interface>
      <!-- interface-requires gtk+ 3.0 -->
      <menu id="menubar">
        <submenu>
          <attribute name="label" translatable="yes">_File</attribute>
          <section>
            <item>
              <attribute name="label" translatable="yes">New _Tab</attribute>
              <attribute name="action">app.newtab</attribute>
            </item>
          </section>
          <section>
            <item>
              <attribute name="label" translatable="yes">_Close Tab</attribute>
              <attribute name="action">app.closetab</attribute>
            </item>
            <item>
              <attribute name="label" translatable="yes">_Quit</attribute>
              <attribute name="action">app.quit</attribute>
            </item>
          </section>
        </submenu>
        <submenu>
          <attribute name="label" translatable="yes">_Help</attribute>
          <item>
            <attribute name="label" translatable="yes">_About</attribute>
            <attribute name="action">app.about</attribute>
          </item>
        </submenu>
      </menu>
    </interface>
   |]

menuText :: Text
menuText = toStrict $ renderText def menuDoc

aboutDoc :: Document
aboutDoc =
  [xmlRaw|
    <?xml version="1.0"?>
    <interface>
    <!-- interface-requires gtk+ 3.8 -->
      <object id="aboutDialog" class="GtkDialog">
        <property name="title" translatable="yes">About</property>
        <property name="resizable">False</property>
        <property name="modal">True</property>
        <child internal-child="vbox">
          <object class="GtkBox" id="vbox">
            <child>
              <object class="GtkGrid" id="grid">
                <property name="visible">True</property>
                <property name="margin">6</property>
                <property name="row-spacing">12</property>
                <property name="column-spacing">6</property>
                <child>
                  <object class="GtkLabel" id="fontlabel">
                    <property name="visible">True</property>
                    <property name="label">_Font:</property>
                    <property name="use-underline">True</property>
                    <property name="mnemonic-widget">font</property>
                    <property name="xalign">1</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkFontButton" id="font">
                    <property name="visible">True</property>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">0</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkLabel" id="transitionlabel">
                    <property name="visible">True</property>
                    <property name="label">_Transition:</property>
                    <property name="use-underline">True</property>
                    <property name="mnemonic-widget">transition</property>
                    <property name="xalign">1</property>
                  </object>
                  <packing>
                    <property name="left-attach">0</property>
                    <property name="top-attach">1</property>
                  </packing>
                </child>
                <child>
                  <object class="GtkComboBoxText" id="transition">
                    <property name="visible">True</property>
                    <items>
                      <item translatable="yes" id="none">None</item>
                      <item translatable="yes" id="crossfade">Fade</item>
                      <item translatable="yes" id="slide-left-right">Slide</item>
                    </items>
                  </object>
                  <packing>
                    <property name="left-attach">1</property>
                    <property name="top-attach">1</property>
                  </packing>
                </child>
              </object>
            </child>
          </object>
        </child>
      </object>
    </interface>
   |]

aboutText :: Text
aboutText = toStrict $ renderText def aboutDoc
