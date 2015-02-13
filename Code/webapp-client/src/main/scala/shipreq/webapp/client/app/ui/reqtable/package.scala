package shipreq.webapp.client.app.ui

/**
 * Requirements Table.
 * "Common Req View & Editor" in the prototype.
 *
 * An Excel-like table for reading and editing requirements.
 */
package object reqtable {

  case class ViewSettings(columns: Vector[Column],
                          order  : SortCriteria)
}
