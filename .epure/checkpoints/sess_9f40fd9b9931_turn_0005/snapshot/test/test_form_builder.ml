(******************************************************************************)
(*                                                                            *)
(* SPDX-License-Identifier: MIT                                               *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                 *)
(*                                                                            *)
(******************************************************************************)

(** Unit tests for form_builder.ml core functionality.
    
    Tests the form builder infrastructure by creating complete forms
    and testing their behavior through the spec API. *)

open Alcotest
module FB = Octez_manager_ui.Form_builder

(******************************************************************************)
(*                             TEST MODEL                                    *)
(******************************************************************************)

(** Simple test model for field testing *)
type test_model = {name : string; email : string; enabled : bool; role : string}

let empty_model () = {name = ""; email = ""; enabled = false; role = "user"}

(******************************************************************************)
(*                         FORM SPEC TESTS                                   *)
(******************************************************************************)

(** Test: Form spec can be created with initial model *)
let test_form_spec_creation () =
  let spec : test_model FB.spec =
    {
      title = "Test Form";
      initial_model = empty_model;
      fields =
        (fun _m ->
          [
            FB.text
              ~label:"Name"
              ~get:(fun m -> m.name)
              ~set:(fun v m -> {m with name = v});
          ]);
      on_init = None;
      on_refresh = None;
      pre_submit = None;
      pre_submit_modal = None;
      on_submit = (fun _m -> Ok ());
    }
  in
  check string "spec has title" "Test Form" spec.title ;
  let model = spec.initial_model () in
  check string "initial_model creates empty" "" model.name

(** Test: Dynamic fields can be generated from model *)
let test_dynamic_fields () =
  let spec : test_model FB.spec =
    {
      title = "Dynamic Form";
      initial_model = empty_model;
      fields =
        (fun m ->
          let base_fields =
            [
              FB.text
                ~label:"Name"
                ~get:(fun m -> m.name)
                ~set:(fun v m -> {m with name = v});
            ]
          in
          (* Conditionally add email field only if name is not empty *)
          if String.length m.name > 0 then
            base_fields
            @ [
                FB.text
                  ~label:"Email"
                  ~get:(fun m -> m.email)
                  ~set:(fun v m -> {m with email = v});
              ]
          else base_fields);
      on_init = None;
      on_refresh = None;
      pre_submit = None;
      pre_submit_modal = None;
      on_submit = (fun _m -> Ok ());
    }
  in

  (* With empty name, should have 1 field *)
  let fields_empty = spec.fields (empty_model ()) in
  check int "empty model has 1 field" 1 (List.length fields_empty) ;

  (* With name set, should have 2 fields *)
  let model_with_name = {(empty_model ()) with name = "Alice"} in
  let fields_filled = spec.fields model_with_name in
  check int "filled model has 2 fields" 2 (List.length fields_filled)

(** Test: Form submission validation with pre_submit *)
let test_form_pre_submit_validation () =
  let spec : test_model FB.spec =
    {
      title = "Validated Form";
      initial_model = empty_model;
      fields =
        (fun _m ->
          [
            FB.text
              ~label:"Name"
              ~get:(fun m -> m.name)
              ~set:(fun v m -> {m with name = v});
          ]);
      on_init = None;
      on_refresh = None;
      pre_submit =
        Some
          (fun m ->
            if String.length m.name = 0 then Error (`Msg "Name is required")
            else Ok ());
      pre_submit_modal = None;
      on_submit = (fun _m -> Ok ());
    }
  in

  (* Empty model should fail pre_submit *)
  (match spec.pre_submit with
  | Some f -> (
      match f (empty_model ()) with
      | Ok () -> fail "pre_submit should have failed"
      | Error (`Msg msg) -> check string "error message" "Name is required" msg
      | Error (`Modal _) -> fail "unexpected modal")
  | None -> fail "pre_submit should be Some") ;

  (* Valid model should pass pre_submit *)
  let valid_model = {(empty_model ()) with name = "Alice"} in
  match spec.pre_submit with
  | Some f -> (
      match f valid_model with
      | Ok () -> ()
      | Error _ -> fail "pre_submit should have passed")
  | None -> fail "pre_submit should be Some"

(** Test: Form submission handler *)
let test_form_on_submit () =
  let submitted = ref false in
  let submitted_name = ref "" in

  let spec : test_model FB.spec =
    {
      title = "Submit Form";
      initial_model = empty_model;
      fields =
        (fun _m ->
          [
            FB.text
              ~label:"Name"
              ~get:(fun m -> m.name)
              ~set:(fun v m -> {m with name = v});
          ]);
      on_init = None;
      on_refresh = None;
      pre_submit = None;
      pre_submit_modal = None;
      on_submit =
        (fun m ->
          submitted := true ;
          submitted_name := m.name ;
          Ok ());
    }
  in

  let model = {(empty_model ()) with name = "Bob"} in
  match spec.on_submit model with
  | Ok () ->
      check bool "on_submit called" true !submitted ;
      check string "on_submit received model" "Bob" !submitted_name
  | Error _ -> fail "on_submit should succeed"

(** Test: Multiple fields in form *)
let test_multiple_fields () =
  let spec : test_model FB.spec =
    {
      title = "Multi-Field Form";
      initial_model = empty_model;
      fields =
        (fun _m ->
          [
            FB.text
              ~label:"Name"
              ~get:(fun m -> m.name)
              ~set:(fun v m -> {m with name = v});
            FB.text
              ~label:"Email"
              ~get:(fun m -> m.email)
              ~set:(fun v m -> {m with email = v});
            FB.toggle
              ~label:"Enabled"
              ~get:(fun m -> m.enabled)
              ~set:(fun v m -> {m with enabled = v});
          ]);
      on_init = None;
      on_refresh = None;
      pre_submit = None;
      pre_submit_modal = None;
      on_submit = (fun _m -> Ok ());
    }
  in

  let fields = spec.fields (empty_model ()) in
  check int "has 3 fields" 3 (List.length fields)

(** Test: Form with validated text field *)
let test_validated_form () =
  let spec : test_model FB.spec =
    {
      title = "Validated Form";
      initial_model = empty_model;
      fields =
        (fun _m ->
          [
            FB.validated_text
              ~label:"Email"
              ~get:(fun m -> m.email)
              ~set:(fun v m -> {m with email = v})
              ~validate:(fun m ->
                if String.contains m.email '@' then Ok ()
                else Error "Email must contain @");
          ]);
      on_init = None;
      on_refresh = None;
      pre_submit = None;
      pre_submit_modal = None;
      on_submit = (fun _m -> Ok ());
    }
  in

  let fields = spec.fields (empty_model ()) in
  check int "has validated field" 1 (List.length fields)

(** Test: Form with choice field *)
let test_choice_field_form () =
  let spec : test_model FB.spec =
    {
      title = "Choice Form";
      initial_model = empty_model;
      fields =
        (fun _m ->
          [
            FB.choice
              ~label:"Role"
              ~get:(fun m -> m.role)
              ~set:(fun v m -> {m with role = v})
              ~to_string:(fun x -> x)
              ~items:["user"; "admin"; "moderator"];
          ]);
      on_init = None;
      on_refresh = None;
      pre_submit = None;
      pre_submit_modal = None;
      on_submit = (fun _m -> Ok ());
    }
  in

  let fields = spec.fields (empty_model ()) in
  check int "has choice field" 1 (List.length fields)

(** Test: Form with readonly field *)
let test_readonly_field_form () =
  let spec : test_model FB.spec =
    {
      title = "Readonly Form";
      initial_model = empty_model;
      fields =
        (fun _m ->
          [FB.readonly ~label:"Status" ~get:(fun m -> "User: " ^ m.name)]);
      on_init = None;
      on_refresh = None;
      pre_submit = None;
      pre_submit_modal = None;
      on_submit = (fun _m -> Ok ());
    }
  in

  let fields = spec.fields (empty_model ()) in
  check int "has readonly field" 1 (List.length fields)

(** Test: Form with_hint *)
let test_field_with_hint () =
  let spec : test_model FB.spec =
    {
      title = "Hint Form";
      initial_model = empty_model;
      fields =
        (fun _m ->
          [
            FB.text
              ~label:"Name"
              ~get:(fun m -> m.name)
              ~set:(fun v m -> {m with name = v})
            |> FB.with_hint "Enter your full name";
          ]);
      on_init = None;
      on_refresh = None;
      pre_submit = None;
      pre_submit_modal = None;
      on_submit = (fun _m -> Ok ());
    }
  in

  let fields = spec.fields (empty_model ()) in
  check int "has field with hint" 1 (List.length fields)

(** Test: on_init hook is available *)
let test_on_init_hook () =
  let init_called = ref false in

  let spec : test_model FB.spec =
    {
      title = "Init Hook Form";
      initial_model = empty_model;
      fields = (fun _m -> []);
      on_init = Some (fun _m -> init_called := true);
      on_refresh = None;
      pre_submit = None;
      pre_submit_modal = None;
      on_submit = (fun _m -> Ok ());
    }
  in

  (* Call on_init if it exists *)
  match spec.on_init with
  | Some f ->
      f (empty_model ()) ;
      check bool "on_init called" true !init_called
  | None -> fail "on_init should be Some"

(** Test: on_refresh hook is available *)
let test_on_refresh_hook () =
  let refresh_count = ref 0 in

  let spec : test_model FB.spec =
    {
      title = "Refresh Hook Form";
      initial_model = empty_model;
      fields = (fun _m -> []);
      on_init = None;
      on_refresh = Some (fun _m -> incr refresh_count);
      pre_submit = None;
      pre_submit_modal = None;
      on_submit = (fun _m -> Ok ());
    }
  in

  (* Call on_refresh multiple times *)
  match spec.on_refresh with
  | Some f ->
      f (empty_model ()) ;
      f (empty_model ()) ;
      f (empty_model ()) ;
      check int "on_refresh called 3 times" 3 !refresh_count
  | None -> fail "on_refresh should be Some"

(******************************************************************************)
(*                              TEST SUITE                                   *)
(******************************************************************************)

let () =
  run
    "Form Builder"
    [
      ( "Form Spec",
        [
          test_case "form spec creation" `Quick test_form_spec_creation;
          test_case "dynamic fields" `Quick test_dynamic_fields;
          test_case
            "pre_submit validation"
            `Quick
            test_form_pre_submit_validation;
          test_case "on_submit handler" `Quick test_form_on_submit;
          test_case "multiple fields" `Quick test_multiple_fields;
          test_case "validated form" `Quick test_validated_form;
          test_case "choice field form" `Quick test_choice_field_form;
          test_case "readonly field form" `Quick test_readonly_field_form;
          test_case "field with hint" `Quick test_field_with_hint;
          test_case "on_init hook" `Quick test_on_init_hook;
          test_case "on_refresh hook" `Quick test_on_refresh_hook;
        ] );
    ]
