/* Cairo in Vala sample code */

using GLib;
using Gtk;
using Cairo;

public class CairoSample : Gtk.Window {

    private const int SIZE = 30;

    construct {
        this.title = "Cairo Vala Demo";
        this.destroy += Gtk.main_quit;
        this.set_default_size (450, 550);

        this.create_widgets ();
    }

    private void create_widgets () {
        var drawing_area = new DrawingArea ();
        drawing_area.expose_event += this.on_expose;
        this.add (drawing_area);
    }

    private bool on_expose (DrawingArea da, Gdk.EventExpose event) {
        var ctx = Gdk.cairo_create (da.window);

        ctx.set_source_rgb (0, 0, 0);

        ctx.set_line_width (SIZE / 4);
        ctx.set_tolerance (0.1);

        ctx.set_line_join (LineJoin.ROUND);
        ctx.set_dash (new double[] {SIZE / 4.0, SIZE / 4.0}, 0);
        stroke_shapes (ctx, 0, 0);

        ctx.set_dash (null, 0);
        stroke_shapes (ctx, 0, 3 * SIZE);

        ctx.set_line_join (LineJoin.BEVEL);
        stroke_shapes (ctx, 0, 6 * SIZE);

        ctx.set_line_join (LineJoin.MITER);
        stroke_shapes(ctx, 0, 9 * SIZE);

        fill_shapes (ctx, 0, 12 * SIZE);

        ctx.set_line_join (LineJoin.BEVEL);
        fill_shapes (ctx, 0, 15 * SIZE);
        ctx.set_source_rgb (1, 0, 0);
        stroke_shapes (ctx, 0, 15 * SIZE);

        return true;
    }

    private void stroke_shapes (Context ctx, int x, int y) {
        this.draw_shapes (ctx, x, y, ctx.stroke);
    }

    private void fill_shapes (Context ctx, int x, int y) {
        this.draw_shapes (ctx, x, y, ctx.fill);
    }

    private delegate void DrawMethod ();

    private void draw_shapes (Context ctx, int x, int y, DrawMethod draw_method) {
        ctx.save ();

        ctx.new_path ();
        ctx.translate (x + SIZE, y + SIZE);
        bowtie (ctx);
        draw_method ();

        ctx.new_path ();
        ctx.translate (3 * SIZE, 0);
        square (ctx);
        draw_method ();

        ctx.new_path ();
        ctx.translate (3 * SIZE, 0);
        triangle (ctx);
        draw_method ();

        ctx.new_path ();
        ctx.translate (3 * SIZE, 0);
        inf (ctx);
        draw_method ();

        ctx.restore();
    }

    private void triangle (Context ctx) {
        ctx.move_to (SIZE, 0);
        ctx.rel_line_to (SIZE, 2 * SIZE);
        ctx.rel_line_to (-2 * SIZE, 0);
        ctx.close_path ();
    }

    private void square (Context ctx) {
        ctx.move_to (0, 0);
        ctx.rel_line_to (2 * SIZE, 0);
        ctx.rel_line_to (0, 2 * SIZE);
        ctx.rel_line_to (-2 * SIZE, 0);
        ctx.close_path ();
    }

    private void bowtie (Context ctx) {
        ctx.move_to (0, 0);
        ctx.rel_line_to (2 * SIZE, 2 * SIZE);
        ctx.rel_line_to (-2 * SIZE, 0);
        ctx.rel_line_to (2 * SIZE, -2 * SIZE);
        ctx.close_path ();
    }

    private void inf (Context ctx) {
        ctx.move_to (0, SIZE);
        ctx.rel_curve_to (0, SIZE, SIZE, SIZE, 2 * SIZE, 0);
        ctx.rel_curve_to (SIZE, -SIZE, 2 * SIZE, -SIZE, 2 * SIZE, 0);
        ctx.rel_curve_to (0, SIZE, -SIZE, SIZE, -2 * SIZE, 0);
        ctx.rel_curve_to (-SIZE, -SIZE, -2 * SIZE, -SIZE, -2 * SIZE, 0);
        ctx.close_path ();
    }

    static int main (string[] args) {
        Gtk.init (ref args);

        var cairo_sample = new CairoSample ();
        cairo_sample.show_all ();

        Gtk.main ();

        return 0;
    }
}