package org.sireum.amandroid.xml;
/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
public class AndroidScalaCollection {

  public AndroidScalaCollectionType typ;
  public Object[] elements;

  public AndroidScalaCollection(AndroidScalaCollectionType type, Object[] elements) {
    this.typ = type;
    this.elements = elements;
  }
}