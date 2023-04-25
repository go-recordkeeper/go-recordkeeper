---
title: "Click and Drag"
date: 2023-04-25T18:46:52-04:00
type: post
slug: "27-click-and-drag"
tags:
  - javascript
---

The motivating pain point for filing the [Mobile UX](https://github.com/go-recordkeeper/go-recordkeeper/issues/7) issue was click-and-drag on mobile. In fact, the major pain point with recording games on OGS was fat fingering inputs on my phone, so it's honestly embarrassing that the issue has been around this long.

The problem is that using a mouse fires different DOM input events than using a finger on a touch screen. I was able to easily develop and work with the mouse events because that's how my dev machine worked, but when I was originally writing the board code I didn't bother hooking up my phone. When I realized that entering moves didn't work at all on mobile devices, I spent some time researching the problem, but I was hindered by the fact that I had no useful debugging tools on my phone. I eventually got it mostly working, but clicking and dragging to make fine adjustments did not work, which was annoying.

## Mouse, Pointer, and Touch

There are three general categories of input events that I found to be relevant to the problem:

* [**Mouse**](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent) (`mousedown`, `mousemove`, `mouseup`) - These events are fired when your mouse or touchpad (basically a mouse) does things. These are the events I was already familiar with, so they were my first guess.
* [**Pointer**](https://developer.mozilla.org/en-US/docs/Web/API/PointerEvent#pointer_event_types) (`pointerdown`, `pointerup`, `pointermove`, `pointercancel`) - These events are a superset of the mouse events. According to the docs:

  > A pointer is a hardware agnostic representation of input devices (such as a mouse, pen or contact point on a touch-enable surface).

  I don't have a stylus, but pointer events are indeed fired on mobile devies (sometimes).

  One other weirdity is the [`pointercancel`](https://developer.mozilla.org/en-US/docs/Web/API/Element/pointercancel_event) event. Instead of the simple stream of `down`, `move`, and `up` mouse events, pointer events are assumed to be grouped into some kind of abortable action. The docs mention all kinds of weird hardware corner cases that might cause a "pointing" to be cancelled.

* [**Touch**](https://developer.mozilla.org/en-US/docs/Web/API/Touch_events) (`touchstart`, `touchmove`, `touchend`) - These events are layed out similarly to the mouse/pointer events, but they are specifically designed to support multi-touch displays. Each `TouchEvent` doesn't correspond to a single state change of a single variable. Instead, the browser tracks a number of `Touch` objects that each correspond to a finger touching the screen, and a `TouchEvent` can contain updates to multiple `Touch`es.

## Actual behavior

These three event APIs have a complicated, overlapping Venn diagram thing going on, so it's useful to understand what events are actually fired when on what platform.

On a desktop or laptop using a mouse or trackpad, both mouse and pointer events are generated for clicks as well as clicks and drags. Every mouse event has a corresponding pointer event, so don't listen to both, or you will handle the event twice. No touch events are generated.

On a mobile device, things are more complicated:

* The initial contact with the screen generates a `mousedown` and a `pointerdown` event. No touch events are generated.
* Very small motions, like rocking your finger to the side slightly, generate `pointermove` events, but no `mousemove`. Still no touch events are generated. This will continue indefinitely as long as you don't lift your finger or move too far away from the initial point of contact.
* If you pick your finger at this point, a `pointerup` event is fired. This means that simple taps work fine using just pointer events, which is what my bandaid solution relied on.
* If the motion progresses too far from the initial point of contact, a `pointercancel` is fired, further pointer events cease, and touch events begin happening.

Basically, your touch screen will try to act like a generic pointer as long as you are only tapping the screen, but as soon as you get too cheeky with sliding your finger around, it will transition you to touch events. I assume this is because my phone support multi-touch; perhaps older phones will continue to fire pointer events?

## The solution

Since both devices always fire pointer events or touch events, but never both, I just kept the existing pointer event handlers and added handlers for `touchmove` and `touchend` and everything worked fine.

Mouse and pointer events have `clientX`/`clientY` properties that describe the position of the event relative the window, and also `offsetX`/`offsetY` properties that describe the position of the event relative to the DOM element that caught the event. For whatever reason, the powers that be decided that touch events only get `clientX`/`clientY`, so I had to calculate the `offsetX`/`offsetY` myself, which was obnoxious.

Anyway, it works now!
