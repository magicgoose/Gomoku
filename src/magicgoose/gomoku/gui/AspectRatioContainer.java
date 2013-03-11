package magicgoose.gomoku.gui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;

import javax.swing.JPanel;

/**
 * Simple container for a single component to keep it at a given aspect ratio.
 * If we get an area not matching the aspect ratio, the component is shrunken to
 * the aspect ratio.
 * 
 * Modified by `magicgoose`, original version taken from:
 * http://www.landschoff.net/blog/2009/03/enforcing-the-aspect-ratio-
 * of-a-swing-widget/
 */
@SuppressWarnings("serial")
public class AspectRatioContainer extends JPanel implements LayoutManager {

	/** Aspect ratio to maintain (width / height). */
	private double aspectRatio;

	/** Create a new container with given ratio. */
	public AspectRatioContainer(double aspectRatio) {
		this.aspectRatio = aspectRatio;
		setLayout(this);
	}

	/**
	 * Create a container to display component c at given aspect ratio.
	 * 
	 * @param aspectRatio
	 *            target aspect ratio (width / height)
	 * @param c
	 *            contained component
	 */
	public AspectRatioContainer(double aspectRatio, Component c) {
		this(aspectRatio);
		add(c);
	}

	// ----- Layout manager implementation -----

	@Override
	public void layoutContainer(Container parent) {
		synchronized (parent.getTreeLock()) {
			final Component component = getSingleComponent(parent);
			if (component != null) {
				final Dimension parentSize = parent.getSize();
				final Dimension size = correctAspect(parentSize, false);
				component.setBounds(0/* (parentSize.width - size.width) / 2 */,
						0/* (parentSize.height - size.height) / 2 */,
						size.width, size.height);
			}
		}
	}

	@Override
	public Dimension minimumLayoutSize(Container parent) {
		synchronized (parent.getTreeLock()) {
			Component c = getSingleComponent(parent);
			if (c != null) {
				return correctAspect(c.getMinimumSize(), true);
			} else {
				return new Dimension();
			}
		}
	}

	@Override
	public Dimension preferredLayoutSize(Container parent) {
		synchronized (parent.getTreeLock()) {
			Component c = getSingleComponent(parent);
			if (c != null) {
				return correctAspect(c.getPreferredSize(), true);
			} else {
				return new Dimension();
			}
		}
	}

	@Override
	public void addLayoutComponent(String name, Component comp) {
		// No implementation needed, we get the component directly via
		// getComponent.
	}

	@Override
	public void removeLayoutComponent(Component comp) {
		// No implementation needed, we get the component directly via
		// getComponent.
	}

	/**
	 * We support only a single component, which is returned by this method. If
	 * there is was no managed component added so far, this returns null.
	 * 
	 * @return contained component or null if not set
	 */
	private static Component getSingleComponent(Container parent) {
		assert parent.getComponentCount() <= 1 : "AspectRatioContainer should contain at most one component";
		return parent.getComponentCount() == 0 ? null : parent.getComponent(0);
	}

	/**
	 * Corrects a given dimension wrt. aspect ratio.
	 * 
	 * @param d
	 *            original dimension
	 * @param growToAdjust
	 *            true indicates that space should be added to adjust
	 * @return new dimension, grown/shrunken to match aspect ratio
	 */
	private Dimension correctAspect(Dimension d, boolean growToAdjust) {
		int w = d.width, h = d.height;

		if ((w < h * aspectRatio) == growToAdjust) {
			w = Math.round((float) (h * aspectRatio));
		} else {
			h = Math.round((float) (w / aspectRatio));
		}

		return new Dimension(w, h);
	}
}
