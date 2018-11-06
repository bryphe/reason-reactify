open Lwt;
open LTerm_widget;

let main = () => {
    let (waiter, wakener) = wait();

    let vbox = new vbox;
    let button = (new button)(~brackets=("[ ", " ]"), "exit");
    let label = (new label)("_");
    button#on_click(wakeup(wakener));
    vbox#add(button);
    vbox#add(label);

    for (i in 0 to 2) {
        let hbox = new hbox;
        let button = i => {
            let button = (new button)("button" ++ string_of_int(i));
            button#on_click(() => label#set_text(string_of_int(i)));
            button;
        };

        hbox#add(button(i * 3 + 1));
        hbox#add(~expand=false, new vline);
        hbox#add(button(i * 3 + 2));
        hbox#add(~expand=false, new vline);
        hbox#add(button(i * 3 + 3));
        vbox#add(~expand=false, new hline);
        vbox#add(hbox);
    };

    let frame = new frame;
    frame#set(vbox);
    frame#set_label(~alignment=LTerm_geom.H_align_center, "Button test");

    Lazy.force(LTerm.stdout)
        >>= (
            term =>
            LTerm.enable_mouse(term)
            >>= (
                () =>
                Lwt.finalize(
                    () => run(term, frame, waiter),
                    () => LTerm.disable_mouse(term),
                )
            )
        );
};

let () = Lwt_main.run(main());
